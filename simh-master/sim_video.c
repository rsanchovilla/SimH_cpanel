
/* sim_video.c: Bitmap video output 

   Copyright (c) 2011-2013, Matt Burke

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
   THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the name of the author shall not be
   used in advertising or otherwise to promote the sale, use or other dealings
   in this Software without prior written authorization from the author.

   Dic-2021     RSV     Added support for multiple windows (sync with sim_video.c at SimH repo)
                        Added bilinear scaling for very nice displaying
                        reduce mem usage, improve scaling performance
   Jun-2021     RSV     Added support for incremental updates based on
                        rectangle list to greatly reduce CPU usage and
                        allow scale down with antialiasing
                        (identified & isolated with ifdef's)
   Jun-2020     RSV     Some additions & changes for Control Panel support
                        (identified & isolated with ifdef's)
   08-Nov-2013  MB      Added globals for current mouse status
   11-Jun-2013  MB      First version
*/

#include "sim_video.h"
#include "scp.h"

int vid_active = 0;
int32 vid_cursor_x;
int32 vid_cursor_y;
t_bool vid_mouse_b1 = FALSE;
t_bool vid_mouse_b2 = FALSE;
t_bool vid_mouse_b3 = FALSE;
static VID_QUIT_CALLBACK vid_quit_callback = NULL;
static VID_GAMEPAD_CALLBACK motion_callback[10];
static VID_GAMEPAD_CALLBACK button_callback[10];
static int vid_gamepad_inited = 0;

t_stat vid_register_quit_callback (VID_QUIT_CALLBACK callback)
{
vid_quit_callback = callback;
return SCPE_OK;
}

static t_stat register_callback (void **array, int n, void *callback)
{
    int i, j = -1;

    if (!vid_gamepad_inited) {
        return SCPE_NOATT;
        }

    for (i = 0; i < n; i++) {
        if (array[i] == callback)
            return SCPE_ALATT;
        if (array[i] == NULL)
            j = i;
        }

    if (j != -1) {
        array[j] = callback;
        return SCPE_OK;
        }

    return SCPE_NXM;
}

t_stat vid_register_gamepad_motion_callback (VID_GAMEPAD_CALLBACK callback)
{
    int n = sizeof (motion_callback) / sizeof (callback);
    return register_callback ((void **)motion_callback, n, (void *)callback);
}

t_stat vid_register_gamepad_button_callback (VID_GAMEPAD_CALLBACK callback)
{
    int n = sizeof (button_callback) / sizeof (callback);
    return register_callback ((void **)button_callback, n, (void *)callback);
}

t_stat vid_show (FILE* st, DEVICE *dptr,  UNIT* uptr, int32 val, CONST char* desc)
{
return vid_show_video (st, uptr, val, desc);
}

#if defined(USE_SIM_VIDEO) && defined(HAVE_LIBSDL)

static const char *vid_dname (DEVICE *dev)
{
return dev ? sim_dname(dev) : "Video Device";
}

static int vid_gamepad_ok = 0; /* Or else just joysticks. */

char vid_release_key[64] = "Ctrl-Right-Shift";

#include <SDL.h>
#include <SDL_thread.h>

static const char *key_names[] = 
    {"F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", 
     "0",   "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",    
     "A",   "B",  "C",  "D",  "E",  "F",  "G",  "H",  "I",  "J", 
     "K",   "L",  "M",  "N",  "O",  "P",  "Q",  "R",  "S",  "T", 
     "U",   "V",  "W",  "X",  "Y",  "Z", 
     "BACKQUOTE",   "MINUS",   "EQUALS", "LEFT_BRACKET", "RIGHT_BRACKET", 
     "SEMICOLON", "SINGLE_QUOTE", "BACKSLASH", "LEFT_BACKSLASH", "COMMA", 
     "PERIOD", "SLASH", "PRINT", "SCRL_LOCK", "PAUSE", "ESC", "BACKSPACE", 
     "TAB", "ENTER", "SPACE", "INSERT", "DELETE", "HOME", "END", "PAGE_UP", 
     "PAGE_DOWN", "UP", "DOWN", "LEFT", "RIGHT", "CAPS_LOCK", "NUM_LOCK", 
     "ALT_L", "ALT_R", "CTRL_L", "CTRL_R", "SHIFT_L", "SHIFT_R", 
     "WIN_L", "WIN_R", "MENU", "KP_ADD", "KP_SUBTRACT", "KP_END", "KP_DOWN", 
     "KP_PAGE_DOWN", "KP_LEFT", "KP_RIGHT", "KP_HOME", "KP_UP", "KP_PAGE_UP", 
     "KP_INSERT", "KP_DELETE", "KP_5", "KP_ENTER", "KP_MULTIPLY", "KP_DIVIDE"
     };

const char *vid_key_name (uint32 key)
{
static char tmp_key_name[40];

    if (key < sizeof(key_names)/sizeof(key_names[0]))
        sprintf (tmp_key_name, "SIM_KEY_%s", key_names[key]);
    else
        sprintf (tmp_key_name, "UNKNOWN KEY: %d", key);
    return tmp_key_name;
}

#if defined(HAVE_LIBPNG)
/* From: https://github.com/driedfruit/SDL_SavePNG */

/*
 * Save an SDL_Surface as a PNG file.
 *
 * Returns 0 success or -1 on failure, the error message is then retrievable
 * via SDL_GetError().
 */
#define SDL_SavePNG(surface, file) \
        SDL_SavePNG_RW(surface, SDL_RWFromFile(file, "wb"), 1)

/*
 * SDL_SavePNG -- libpng-based SDL_Surface writer.
 *
 * This code is free software, available under zlib/libpng license.
 * http://www.libpng.org/pub/png/src/libpng-LICENSE.txt
 */
#include <SDL.h>
#include <png.h>
#include <zlib.h>

#define SUCCESS 0
#define ERROR -1

#define USE_ROW_POINTERS

#if SDL_BYTEORDER == SDL_BIG_ENDIAN
#define rmask 0xFF000000
#define gmask 0x00FF0000
#define bmask 0x0000FF00
#define amask 0x000000FF
#else
#define rmask 0x000000FF
#define gmask 0x0000FF00
#define bmask 0x00FF0000
#define amask 0xFF000000
#endif

/* libpng callbacks */ 
static void png_error_SDL(png_structp ctx, png_const_charp str)
{
    SDL_SetError("libpng: %s\n", str);
}
static void png_write_SDL(png_structp png_ptr, png_bytep data, png_size_t length)
{
    SDL_RWops *rw = (SDL_RWops*)png_get_io_ptr(png_ptr);
    SDL_RWwrite(rw, data, sizeof(png_byte), length);
}

static SDL_Surface *SDL_PNGFormatAlpha(SDL_Surface *src) 
{
    SDL_Surface *surf;
    SDL_Rect rect = { 0 };

    /* NO-OP for images < 32bpp and 32bpp images that already have Alpha channel */ 
    if (src->format->BitsPerPixel <= 24 || src->format->Amask) {
        src->refcount++;
        return src;
    }

    /* Convert 32bpp alpha-less image to 24bpp alpha-less image */
    rect.w = src->w;
    rect.h = src->h;
    surf = SDL_CreateRGBSurface(src->flags, src->w, src->h, 24,
        src->format->Rmask, src->format->Gmask, src->format->Bmask, 0);
    SDL_LowerBlit(src, &rect, surf, &rect);

    return surf;
}

static int SDL_SavePNG_RW(SDL_Surface *surface, SDL_RWops *dst, int freedst) 
{
    png_structp png_ptr;
    png_infop info_ptr;
    png_colorp pal_ptr;
    SDL_Palette *pal;
    int i, colortype;
#ifdef USE_ROW_POINTERS
    png_bytep *row_pointers;
#endif
    /* Initialize and do basic error checking */
    if (!dst)
    {
        SDL_SetError("Argument 2 to SDL_SavePNG_RW can't be NULL, expecting SDL_RWops*\n");
        return (ERROR);
    }
    if (!surface)
    {
        SDL_SetError("Argument 1 to SDL_SavePNG_RW can't be NULL, expecting SDL_Surface*\n");
        if (freedst) SDL_RWclose(dst);
        return (ERROR);
    }
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, png_error_SDL, NULL); /* err_ptr, err_fn, warn_fn */
    if (!png_ptr) 
    {
        SDL_SetError("Unable to png_create_write_struct on %s\n", PNG_LIBPNG_VER_STRING);
        if (freedst) SDL_RWclose(dst);
        return (ERROR);
    }
    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr)
    {
        SDL_SetError("Unable to png_create_info_struct\n");
        png_destroy_write_struct(&png_ptr, NULL);
        if (freedst) SDL_RWclose(dst);
        return (ERROR);
    }
    if (setjmp(png_jmpbuf(png_ptr)))    /* All other errors, see also "png_error_SDL" */
    {
        png_destroy_write_struct(&png_ptr, &info_ptr);
        if (freedst) SDL_RWclose(dst);
        return (ERROR);
    }

    /* Setup our RWops writer */
    png_set_write_fn(png_ptr, dst, png_write_SDL, NULL); /* w_ptr, write_fn, flush_fn */

    /* Prepare chunks */
    colortype = PNG_COLOR_MASK_COLOR;
    if (surface->format->BytesPerPixel > 0
    &&  surface->format->BytesPerPixel <= 8
    && (pal = surface->format->palette))
    {
        colortype |= PNG_COLOR_MASK_PALETTE;
        pal_ptr = (png_colorp)malloc(pal->ncolors * sizeof(png_color));
        for (i = 0; i < pal->ncolors; i++) {
            pal_ptr[i].red   = pal->colors[i].r;
            pal_ptr[i].green = pal->colors[i].g;
            pal_ptr[i].blue  = pal->colors[i].b;
        }
        png_set_PLTE(png_ptr, info_ptr, pal_ptr, pal->ncolors);
        free(pal_ptr);
    }
    else if (surface->format->BytesPerPixel > 3 || surface->format->Amask)
        colortype |= PNG_COLOR_MASK_ALPHA;

    png_set_IHDR(png_ptr, info_ptr, surface->w, surface->h, 8, colortype,
        PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

//    png_set_packing(png_ptr);

    /* Allow BGR surfaces */
    if (surface->format->Rmask == bmask
    && surface->format->Gmask == gmask
    && surface->format->Bmask == rmask)
        png_set_bgr(png_ptr);

    /* Write everything */
    png_write_info(png_ptr, info_ptr);
#ifdef USE_ROW_POINTERS
    row_pointers = (png_bytep*) malloc(sizeof(png_bytep)*surface->h);
    for (i = 0; i < surface->h; i++)
        row_pointers[i] = (png_bytep)(Uint8*)surface->pixels + i * surface->pitch;
    png_write_image(png_ptr, row_pointers);
    free(row_pointers);
#else
    for (i = 0; i < surface->h; i++)
        png_write_row(png_ptr, (png_bytep)(Uint8*)surface->pixels + i * surface->pitch);
#endif
    png_write_end(png_ptr, info_ptr);

    /* Done */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    if (freedst) SDL_RWclose(dst);
    return (SUCCESS);
}
#endif /* defined(HAVE_LIBPNG) */

/* 
    Some platforms (OS X), require that ALL input event processing be 
    performed by the main thread of the process.

    To satisfy this requirement, we leverage the SDL_MAIN functionality 
    which does:
    
             #defines main SDL_main
     
     and we define the main() entry point here.  Locally, we run the 
     application's SDL_main in a separate thread, and while that thread
     is running, the main thread performs event handling and dispatch.
     
 */

#define EVENT_REDRAW     1                              /* redraw event for SDL */
#define EVENT_CLOSE      2                              /* close event for SDL */
#define EVENT_CURSOR     3                              /* new cursor for SDL */
#define EVENT_WARP       4                              /* warp mouse position for SDL */
#define EVENT_DRAW       5                              /* draw/blit region for SDL */
#define EVENT_SHOW       6                              /* show SDL capabilities */
#define EVENT_OPEN       7                              /* vid_open request */
#define EVENT_EXIT       8                              /* program exit */
#define EVENT_SCREENSHOT 9                              /* produce screenshot of video window */
#define EVENT_BEEP      10                              /* audio beep */
#if defined(CPANEL)
#define EVENT_SIZEANDPOS 11                             /* set windows size and/or pos */

#define CP_SDL          0x00000020                      
#define CP_CMDS         0x00000010                      

// on main SDL thread, do not call sim_debug () macro. This macro is NOT thread-safe, so will (probably) 
// crash the simulation when both main simulation and SDL threads executes at the same time macro code
// for logging into main SDL thread, use sim_debug_cp () macro
extern char sim_debug_cp_buf[1024] = "SDL: ---";        
#define sim_debug_cp(...) do { if (sim_deb && vptr->vid_dev && ((vptr->vid_dev)->dctrl & (CP_SDL))) {sprintf (sim_debug_cp_buf+5, __VA_ARGS__);fwrite (sim_debug_cp_buf, 1, strlen(sim_debug_cp_buf), sim_deb);}} while (0)

static const char *usereventtypes[12] = {"(None)",
                                          "REDRAW", "CLOSE","CURSOR","WARP",
                                          "DRAW","SHOW","OPEN","EXIT",
                                          "SCREENSHOT","BEEP","SIZEANDPOS" };
struct {
    int x,y; 
    int Mode; // 0=none, 1=set pos, 2=set size, 3=set pos relative, 4=set window title
    int InProgress; 
} WindowSizeAndPos; 

void vid_SetWindowSizeAndPos_event (VID_DISPLAY * vptr);

struct {
    int ongoing_flag;  // support for drag control panel window
    int x, y;          // position of mouse in window at start of drag
    VID_DISPLAY *vptr; // windows being dragged
} window_drag;

void vid_update_cursor (VID_DISPLAY *vptr, SDL_Cursor *cursor, int visible); 
void vid_refresh_ex (VID_DISPLAY *vptr, uint32 * pixels, void * RectList);
int vid_refresh_in_progress = 0; 

#define TOOLTIP_WIDTH   200
#define TOOLTIP_HEIGHT  150
struct {
    int visible;
    SDL_Window *window;    
    SDL_Renderer *renderer;
    SDL_Texture *texture;
    uint32 * surface; 
    VID_DISPLAY * parent_vptr; 
    int hide_requested;
    int x_pos, y_pos; 
} tooltip = {0,NULL}; 
void vid_update_tooltip(void); 
void ShowTooltipAtMousePos(int x_pos, int y_pos, VID_DISPLAY * vptr);

char DropFile_FileName[1024] = ""; // file dropped
int DropFile_x_pos, DropFile_y_pos; // pos where file is dropped in
VID_DISPLAY *DropFile_vptr = NULL; // windows where file is dropped in
void adjust_xy_to_current_scale(VID_DISPLAY * vptr, int * x, int * y); 

vidkeyb vid_keyb = {0}; 

#else
#define sim_debug_cp(...) if (0) printf (__VA_ARGS__)
static const char *usereventtypes[1];

#endif
#define MAX_EVENTS      20                              /* max events in queue */

typedef struct {
    SIM_KEY_EVENT events[MAX_EVENTS];
    SDL_sem *sem;
    int32 head;
    int32 tail;
    int32 count;
    } KEY_EVENT_QUEUE;

typedef struct {
    SIM_MOUSE_EVENT events[MAX_EVENTS];
    SDL_sem *sem;
    int32 head;
    int32 tail;
    int32 count;
    } MOUSE_EVENT_QUEUE;

int vid_thread (void* arg);
int vid_video_events (VID_DISPLAY *vptr);
void vid_show_video_event (void);
void vid_screenshot_event (void);
void vid_beep_event (void);
static void vid_beep_setup (int duration_ms, int tone_frequency);
static void vid_beep_cleanup (void);
static void vid_controllers_setup (DEVICE *dptr);
static void vid_controllers_cleanup (void);

struct VID_DISPLAY {
   t_bool vid_active_window;
   t_bool vid_mouse_captured;
   int32 vid_flags;                                        /* Open Flags */
   int32 vid_width;                                        // size of window at creation. Will never change
   int32 vid_height;                                       // the size of vid_surface; 
#if defined(CPANEL)
   uint32 *pixels;                                         // pointer to pixels of window. expected size is sizeof(uint32) * vid_width * vid_height
   int scale;                                              // scale of window. 10..200. 50% -> texture is 50% of size of surface
   int32 vid_texture_width;                                // size of texture. = vid_width * scale (if scale <= 100)
   int32 vid_texture_height;                               //                  = vid_width (if scale >= 100)
   rectlist *RectList;                                     // pointer to rectangle list: indicate rectangles in pixels array to upadte on screen at given scale
#endif
   t_bool vid_ready;
   char vid_title[128];
   SDL_Texture *vid_texture;                               /* video buffer in GPU */
   SDL_Renderer *vid_renderer;
   SDL_Window *vid_window;                                 /* window handle */
   SDL_PixelFormat *vid_format;
   uint32 vid_windowID;                                    // The numeric ID is what SDL_WindowEvent references
   SDL_mutex *vid_draw_mutex;                              /* window update mutex */
   SDL_Cursor *vid_cursor;                                 /* current cursor */
   t_bool vid_cursor_visible;                              /* cursor visibility state */
   DEVICE *vid_dev;
   t_bool vid_key_state[SDL_NUM_SCANCODES];
   VID_DISPLAY *next;
   t_bool vid_blending;
   SDL_Rect *vid_dst_last;
   uint32 *vid_data_last;
};

SDL_Thread *vid_thread_handle = NULL;                   /* event thread handle */

static VID_DISPLAY vid_first;

KEY_EVENT_QUEUE vid_key_events;                         /* keyboard events */
MOUSE_EVENT_QUEUE vid_mouse_events;                     /* mouse events */

static VID_DISPLAY *vid_get_event_window (SDL_Event *ev, Uint32 windowID)
{
static Uint32 lastID = 0xffffffff;
static VID_DISPLAY *last_display = NULL;
VID_DISPLAY *vptr;
SDL_KeyboardEvent *kev;
SDL_MouseButtonEvent *bev;
SDL_MouseMotionEvent *mev;
SDL_WindowEvent *wev;
SDL_UserEvent *uev;

#if defined(CPANEL)
   if (ev == NULL) return NULL; 
   // discard events on tooltip window
   if ((tooltip.window) && (ev->window.windowID==SDL_GetWindowID (tooltip.window))) return NULL; 
   // possible bug in original sim_video.c 
   // check EVENT_OPEN: it holds vptr in user.data1, instead of having to search it with vid_windowID not yet created
   if ((ev->type==SDL_USEREVENT) && (ev->user.code==EVENT_OPEN)) return (VID_DISPLAY *) ev->user.data1; 
   // EVENT_CURSOR is system wide. Tie it to first window
   if ((ev->type==SDL_USEREVENT) && (ev->user.code==EVENT_CURSOR))        return (VID_DISPLAY *) &vid_first; 
#endif

if (windowID == lastID)
    return last_display;

for (vptr = &vid_first; vptr != NULL; vptr = vptr->next) {
    if (windowID == vptr->vid_windowID) {
        lastID = windowID;
        return last_display = vptr;
        }
    }

#if defined(CPANEL)
   if ((ev->type==SDL_WINDOWEVENT) && (ev->user.code==SDL_WINDOWEVENT_LEAVE)) {
       // windows leave event arriving late, when window has already been closed
       return NULL; 
   }
#endif

switch (ev->type) {
    case SDL_KEYDOWN:
    case SDL_KEYUP:
        kev = (SDL_KeyboardEvent *)ev;
        sim_messagef (SCPE_OK, "Unrecognized key event.\n");
        sim_messagef (SCPE_OK, "  type = %u\n", kev->type);
        sim_messagef (SCPE_OK, "  timestamp = %u\n", kev->timestamp);
        sim_messagef (SCPE_OK, "  windowID = %u\n", kev->windowID);
        sim_messagef (SCPE_OK, "  state = %u\n", kev->state);
        sim_messagef (SCPE_OK, "  repeat = %u\n", kev->repeat);
        sim_messagef (SCPE_OK, "  scancode = %d\n", kev->keysym.scancode);
        sim_messagef (SCPE_OK, "  sym = %d\n", kev->keysym.sym);
        sim_messagef (SCPE_OK, "  mod = %u\n", kev->keysym.mod);
        break;
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
        bev = (SDL_MouseButtonEvent *)ev;
        sim_messagef (SCPE_OK, "Unrecognized mouse button event.\n");
        sim_messagef (SCPE_OK, "  type = %u\n", bev->type);
        sim_messagef (SCPE_OK, "  timestamp = %u\n", bev->timestamp);
        sim_messagef (SCPE_OK, "  windowID = %u\n", bev->windowID);
        sim_messagef (SCPE_OK, "  which = %u\n", bev->which);
        sim_messagef (SCPE_OK, "  button = %u\n", bev->button);
        sim_messagef (SCPE_OK, "  state = %u\n", bev->state);
        sim_messagef (SCPE_OK, "  clicks = %u\n", bev->clicks);
        sim_messagef (SCPE_OK, "  x = %d\n", bev->x);
        sim_messagef (SCPE_OK, "  y = %d\n", bev->y);
        break;
    case SDL_MOUSEMOTION:
        mev = (SDL_MouseMotionEvent *)ev;
        sim_messagef (SCPE_OK, "Unrecognized mouse motion event.\n");
        sim_messagef (SCPE_OK, "  type = %u\n", mev->type);
        sim_messagef (SCPE_OK, "  timestamp = %u\n", mev->timestamp);
        sim_messagef (SCPE_OK, "  windowID = %u\n", mev->windowID);
        sim_messagef (SCPE_OK, "  which = %u\n", mev->which);
        sim_messagef (SCPE_OK, "  state = %u\n", mev->state);
        sim_messagef (SCPE_OK, "  x = %d\n", mev->x);
        sim_messagef (SCPE_OK, "  y = %d\n", mev->y);
        sim_messagef (SCPE_OK, "  xrel = %d\n", mev->xrel);
        sim_messagef (SCPE_OK, "  yrel = %d\n", mev->yrel);
        break;
    case SDL_WINDOWEVENT:
        wev = (SDL_WindowEvent *)ev;
        sim_messagef (SCPE_OK, "Unrecognized window event.\n");
        sim_messagef (SCPE_OK, "  type = %u\n", wev->type);
        sim_messagef (SCPE_OK, "  timestamp = %u\n", wev->timestamp);
        sim_messagef (SCPE_OK, "  windowID = %u\n", wev->windowID);
        sim_messagef (SCPE_OK, "  event = %u\n", wev->event);
        sim_messagef (SCPE_OK, "  data1 = %d\n", wev->data1);
        sim_messagef (SCPE_OK, "  data2 = %d\n", wev->data2);
        break;
    case SDL_USEREVENT:
        uev = (SDL_UserEvent *)ev;
        sim_messagef (SCPE_OK, "Unrecognized user event.\n");
        sim_messagef (SCPE_OK, "  type = %u\n", uev->type);
        sim_messagef (SCPE_OK, "  timestamp = %u\n", uev->timestamp);
        sim_messagef (SCPE_OK, "  windowID = %u\n", uev->windowID);
        sim_messagef (SCPE_OK, "  code = %d\n", uev->code);
        sim_messagef (SCPE_OK, "  data1 = %p\n", uev->data1);
        sim_messagef (SCPE_OK, "  data2 = %p\n", uev->data2);
        break;
    default:
        sim_messagef (SCPE_OK, "Unrecognized event type %u\n", ev->type);
        break;
    }

sim_messagef (SCPE_OK,
"\nSIMH has encountered a bug in SDL2.  An upgrade to SDL2\n"
"version 2.0.14 should fix this problem.\n");

return NULL;
}

#if defined (SDL_MAIN_AVAILABLE)
#if defined (main)
#undef main
#endif

static int main_argc;
static char **main_argv;
static SDL_Thread *vid_main_thread_handle;

int main_thread (void *arg)
{
SDL_Event user_event;
int stat;

stat = SDL_main (main_argc, main_argv);
user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_EXIT;
user_event.user.data1 = NULL;
user_event.user.data2 = NULL;
while (SDL_PushEvent (&user_event) < 0)
    sim_os_ms_sleep (10);
return stat;
}

int main (int argc, char *argv[])
{
SDL_Event event;
int status;

main_argc = argc;
main_argv = argv;

SDL_SetHint (SDL_HINT_RENDER_DRIVER, "software");

status = SDL_Init (SDL_INIT_VIDEO);

if (status) {
    fprintf (stderr, "SDL Video subsystem can't initialize: %s\n", SDL_GetError ());
    exit (1);
    }

vid_main_thread_handle = SDL_CreateThread (main_thread , "simh-main", NULL);

if (vid_main_thread_handle == NULL) {
    fprintf (stderr, "SDL_CreateThread failed: %s\n", SDL_GetError ());
    exit (1);
    }

vid_beep_setup (400, 660);

memset (&event, 0, sizeof (event));

while (1) {
    int status = SDL_WaitEvent (&event);
    if (status == 1) {
        if (event.type == SDL_USEREVENT) {
            if (event.user.code == EVENT_EXIT)
                break;
            if (event.user.code == EVENT_OPEN) {
                vid_video_events ((VID_DISPLAY *)event.user.data1);
            } else {
                if (event.user.code == EVENT_SHOW)
                    vid_show_video_event ();
                else {
                    if (event.user.code == EVENT_SCREENSHOT)
                        vid_screenshot_event ();
                    else {
                        sim_printf ("main(): Unexpected User event: %d\n", event.user.code);
                        break;
                        }
                    }
                }
            }
        else {
//          sim_printf ("main(): Ignoring unexpected event: %d\n", event.type);
            }
        }
    else {
        if (status < 0)
            sim_printf ("main() - ` error: %s\n", SDL_GetError());
        }
    }
SDL_WaitThread (vid_main_thread_handle, &status);
vid_beep_cleanup ();
SDL_Quit ();
return status;
}

static t_stat vid_create_window (VID_DISPLAY *vptr)
{
int wait_count = 0;
SDL_Event user_event;

vptr->vid_ready = FALSE;
vptr->vid_window = NULL; // safety
user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_OPEN;
user_event.user.data1 = vptr;
user_event.user.data2 = NULL;

SDL_PushEvent (&user_event);

sim_debug(CP_SDL, vptr->vid_dev, "SimH Thread: vid_create_window '%', w %d, h %d, vptr %x -> Push EVENT_OPEN to Main Thread\n", 
          vptr->vid_title, vptr->vid_width, vptr->vid_height, vptr);

// wait for event processed. Safety: max wait: 30 sec
while ((!vptr->vid_ready) && (++wait_count < 300))  
    sim_os_ms_sleep (100);

if ((!vptr->vid_ready) || (vptr->vid_window == NULL)) {
    // EVENT_OPEN processing has taken more than max wait secs allowed OR error
    // has ocurred while opening window. Instead of closing the window
    // just notify the error
    fprintf (stderr, "EVENT_OPEN processing timeout. vid_create_window failed \r\n");
    //vid_close ();
    return SCPE_OPENERR;
    }
return SCPE_OK;
}
#else
static int vid_create_window (VID_DISPLAY *vptr)
{
int wait_count = 0;

if (vid_thread_handle == NULL)
    vid_thread_handle = SDL_CreateThread (vid_thread, "vid-thread", vptr);
else {
    SDL_Event user_event;
    vptr->vid_ready = FALSE;
    user_event.type = SDL_USEREVENT;
    user_event.user.code = EVENT_OPEN;
    user_event.user.data1 = vptr;
    user_event.user.data2 = NULL;
    SDL_PushEvent (&user_event);
    }

if (vid_thread_handle == NULL) {
    vid_close ();
    return SCPE_OPENERR;
    }
while ((!vptr->vid_ready) && (++wait_count < 20))
    sim_os_ms_sleep (100);
if (!vptr->vid_ready) {
    vid_close ();
    return SCPE_OPENERR;
    }
return SCPE_OK;
}
#endif

static void vid_controllers_setup (DEVICE *dev)
{
SDL_Joystick *y;
SDL_version ver;
int i, n;

if (vid_gamepad_inited++)
    return;

/* Chech that the SDL_GameControllerFromInstanceID function is
   available at run time. */
SDL_GetVersion(&ver);
vid_gamepad_ok = (ver.major > 2 ||
                  (ver.major == 2 && (ver.minor > 0 || ver.patch >= 4)));

if (vid_gamepad_ok)
    SDL_InitSubSystem(SDL_INIT_GAMECONTROLLER);
else
    SDL_InitSubSystem(SDL_INIT_JOYSTICK);

if (SDL_JoystickEventState (SDL_ENABLE) < 0) {
    if (vid_gamepad_ok)
        SDL_QuitSubSystem(SDL_INIT_GAMECONTROLLER);
    else
        SDL_QuitSubSystem(SDL_INIT_JOYSTICK);
    sim_printf ("%s: vid_controllers_setup(): SDL_JoystickEventState error: %s\n", vid_dname(dev), SDL_GetError());
    return;
    }

if (vid_gamepad_ok && SDL_GameControllerEventState (SDL_ENABLE) < 0) {
    if (vid_gamepad_ok)
        SDL_QuitSubSystem(SDL_INIT_GAMECONTROLLER);
    else
        SDL_QuitSubSystem(SDL_INIT_JOYSTICK);
    sim_printf ("%s: vid_controllers_setup(): SDL_GameControllerEventState error: %s\n", vid_dname(dev), SDL_GetError());
    return;
    }

n = SDL_NumJoysticks();

for (i = 0; i < n; i++) {
    if (vid_gamepad_ok && SDL_IsGameController (i)) {
        SDL_GameController *x = SDL_GameControllerOpen (i);
        if (x != NULL) {
            sim_debug (SIM_VID_DBG_VIDEO, dev,
            "Game controller: %s\n", SDL_GameControllerNameForIndex(i));
            }
        }
    else {
        y = SDL_JoystickOpen (i);
        if (y != NULL) {
            sim_debug (SIM_VID_DBG_VIDEO, dev,
            "Joystick: %s\n", SDL_JoystickNameForIndex(i));
            sim_debug (SIM_VID_DBG_VIDEO, dev,
            "Number of axes: %d, buttons: %d\n",
            SDL_JoystickNumAxes(y),
            SDL_JoystickNumButtons(y));
            }
        }
    }
}

static void vid_controllers_cleanup (void)
{
if (0 == (--vid_gamepad_inited)) {
    memset (motion_callback, 0, sizeof motion_callback);
    memset (button_callback, 0, sizeof button_callback);
    if (vid_gamepad_ok)
        SDL_QuitSubSystem(SDL_INIT_GAMECONTROLLER);
    else
        SDL_QuitSubSystem(SDL_INIT_JOYSTICK);
    }
}

static t_stat vid_init_window (VID_DISPLAY *vptr, DEVICE *dptr, const char *title, uint32 width, uint32 height, int flags)
{
t_stat stat;

if ((strlen(sim_name) + 7 + (dptr ? strlen (dptr->name) : 0) + (title ? strlen (title) : 0)) < sizeof (vptr->vid_title))
    sprintf (vptr->vid_title, "%s%s%s%s%s", sim_name, dptr ? " - " : "", dptr ? dptr->name : "", title ? " - " : "", title ? title : "");
else
    sprintf (vptr->vid_title, "%s", sim_name);
vptr->vid_flags = flags;
vptr->vid_active_window = TRUE;
vptr->vid_width = width;
vptr->vid_height = height;
vptr->vid_mouse_captured = FALSE;
vptr->vid_cursor_visible = (vptr->vid_flags & SIM_VID_INPUTCAPTURED);
vptr->vid_blending = FALSE;

#if defined(CPANEL)
sim_debug(CP_SDL, dptr, "SimH Thread: vid_init_window '%s', w %d, h %d, vptr %x \n", 
          vptr->vid_title, vptr->vid_width, vptr->vid_height, vptr);
// possible bug in original sim_video.c vid_cursor was never initialized
vptr->vid_cursor = NULL;
vptr->vid_cursor_visible = 1; // set cursor (=mouse pointer) visible
#endif

if (!vid_active) {
    vid_key_events.head = 0;
    vid_key_events.tail = 0;
    vid_key_events.count = 0;
    vid_key_events.sem = SDL_CreateSemaphore (1);
    vid_mouse_events.head = 0;
    vid_mouse_events.tail = 0;
    vid_mouse_events.count = 0;
    vid_mouse_events.sem = SDL_CreateSemaphore (1);
}

vptr->vid_dev = dptr;

memset (motion_callback, 0, sizeof motion_callback);
memset (button_callback, 0, sizeof button_callback);

stat = vid_create_window (vptr);
if (stat != SCPE_OK)
    return stat;

sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE, vptr->vid_dev, "vid_open() - Success\n");

return SCPE_OK;
}

t_stat vid_open_window (VID_DISPLAY **vptr, DEVICE *dptr, const char *title, uint32 width, uint32 height, int flags)
{
t_stat r;
*vptr = (VID_DISPLAY *)calloc (1, sizeof (VID_DISPLAY));
if (*vptr == NULL)
    return SCPE_NXM;

(*vptr)->next = vid_first.next;
vid_first.next = *vptr;
r = vid_init_window (*vptr, dptr, title, width, height, flags);
if (r != SCPE_OK) {
    vid_first.next = (*vptr)->next;
    free (*vptr);
    *vptr = NULL;
    return r;
    }
return SCPE_OK;
}

t_stat vid_open (DEVICE *dptr, const char *title, uint32 width, uint32 height, int flags)
{
if (!vid_first.vid_active_window)
    return vid_init_window (&vid_first, dptr, title, width, height, flags);
return SCPE_OK;
}

t_stat vid_close_window (VID_DISPLAY *vptr)
{
SDL_Event user_event;
int status;

if (vptr->vid_ready) {
    sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE, vptr->vid_dev, "vid_close()\n");
    user_event.type = SDL_USEREVENT;
    user_event.user.windowID = vptr->vid_windowID;
    user_event.user.code = EVENT_CLOSE;
    user_event.user.data1 = NULL;
    user_event.user.data2 = NULL;

    while (SDL_PushEvent (&user_event) < 0)
        sim_os_ms_sleep (10);
    vptr->vid_dev = NULL;
    }
if (vid_thread_handle && vid_active <= 1) {
    SDL_WaitThread (vid_thread_handle, &status);
    vid_thread_handle = NULL;
    }
while (vptr->vid_ready)
    sim_os_ms_sleep (10);

vptr->vid_active_window = FALSE;
if (!vid_active && vid_mouse_events.sem) {
    SDL_DestroySemaphore(vid_mouse_events.sem);
    vid_mouse_events.sem = NULL;
    }
if (!vid_active && vid_key_events.sem) {
    SDL_DestroySemaphore(vid_key_events.sem);
    vid_key_events.sem = NULL;
    }
return SCPE_OK;
}

t_stat vid_close (void)
{
if (vid_first.vid_active_window)
    return vid_close_window (&vid_first);
return SCPE_OK;
}

t_stat vid_close_all (void)
{
VID_DISPLAY *vptr;
vid_close ();
for (vptr = vid_first.next; vptr != NULL; vptr = vptr->next)
    vid_close_window (vptr);
return SCPE_OK;
}

t_stat vid_poll_kb (SIM_KEY_EVENT *ev)
{
if (SDL_SemTryWait (vid_key_events.sem) == 0) {         /* get lock */
    if (vid_key_events.count > 0) {                     /* events in queue? */
        *ev = vid_key_events.events[vid_key_events.head++];
        vid_key_events.count--;
        if (vid_key_events.head == MAX_EVENTS)
            vid_key_events.head = 0;
        SDL_SemPost (vid_key_events.sem);
        return SCPE_OK;
        }
    SDL_SemPost (vid_key_events.sem);
    }
return SCPE_EOF;
}

t_stat vid_poll_mouse (SIM_MOUSE_EVENT *ev)
{
t_stat stat = SCPE_EOF;
SIM_MOUSE_EVENT *nev;

if (SDL_SemTryWait (vid_mouse_events.sem) == 0) {
    if (vid_mouse_events.count > 0) {
        stat = SCPE_OK;
        *ev = vid_mouse_events.events[vid_mouse_events.head++];
        vid_mouse_events.count--;
        if (vid_mouse_events.head == MAX_EVENTS)
            vid_mouse_events.head = 0;
        nev = &vid_mouse_events.events[vid_mouse_events.head];
        if ((vid_mouse_events.count > 0) &&
            (0 == (ev->x_rel + nev->x_rel)) &&
            (0 == (ev->y_rel + nev->y_rel)) &&
            (ev->b1_state == nev->b1_state) &&
            (ev->b2_state == nev->b2_state) &&
            (ev->b3_state == nev->b3_state)) {
            if ((++vid_mouse_events.head) == MAX_EVENTS)
                vid_mouse_events.head = 0;
            vid_mouse_events.count--;
            stat = SCPE_EOF;
            sim_debug (SIM_VID_DBG_MOUSE, ev->dev, "vid_poll_mouse: ignoring bouncing events\n");
        }
#if defined(CPANEL)
        adjust_xy_to_current_scale(ev->vptr, &ev->x_pos, &ev->y_pos); // adjust coord as if scale is 100% 
#endif
    }
    if (SDL_SemPost (vid_mouse_events.sem))
        sim_printf ("vid_poll_mouse(): SDL_SemPost error: %s\n", SDL_GetError());
    }
return stat;
}

uint32 vid_map_rgb_window (VID_DISPLAY *vptr, uint8 r, uint8 g, uint8 b)
{
return SDL_MapRGB (vptr->vid_format, r, g, b);
}

uint32 vid_map_rgb (uint8 r, uint8 g, uint8 b)
{
return vid_map_rgb_window (&vid_first, r, g, b);
}

uint32 vid_map_rgba_window (VID_DISPLAY *vptr, uint8 r, uint8 g, uint8 b, uint8 a)
{
return SDL_MapRGBA (vptr->vid_format, r, g, b, a);
}

void vid_draw_window (VID_DISPLAY *vptr, int32 x, int32 y, int32 w, int32 h, uint32 *buf)
{
SDL_Event user_event;
SDL_Rect *vid_dst, *last;
uint32 *vid_data;

sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "vid_draw(%d, %d, %d, %d)\n", x, y, w, h);

SDL_LockMutex (vptr->vid_draw_mutex);                         /* Synchronize to check region dimensions */
last = vptr->vid_dst_last;
if (last                               &&               /* As yet unprocessed draw rectangle? */
    (last->x == x) && (last->y == y) &&                 /* AND identical position? */
    (last->w == w) && (last->h == h)) {                 /* AND identical dimensions? */
    memcpy (vptr->vid_data_last, buf, w*h*sizeof(*buf));/* Replace region contents */
    SDL_UnlockMutex (vptr->vid_draw_mutex);                   /* Done */
    return;
    }
SDL_UnlockMutex (vptr->vid_draw_mutex);

vid_dst = (SDL_Rect *)malloc (sizeof(*vid_dst));
if (!vid_dst) {
    sim_printf ("%s: vid_draw() memory allocation error\n", vid_dname(vptr->vid_dev));
    return;
    }
vid_dst->x = x;
vid_dst->y = y;
vid_dst->w = w;
vid_dst->h = h;
vid_data = (uint32 *)malloc (w*h*sizeof(*buf));
if (!vid_data) {
    sim_printf ("%s: vid_draw() memory allocation error\n", vid_dname(vptr->vid_dev));
    free (vid_dst);
    return;
    }
memcpy (vid_data, buf, w*h*sizeof(*buf));
user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_DRAW;
user_event.user.data1 = (void *)vid_dst;
user_event.user.data2 = (void *)vid_data;
SDL_LockMutex (vptr->vid_draw_mutex);         /* protect vid_dst_last & vid_data_last */
vptr->vid_dst_last = vid_dst;
vptr->vid_data_last = vid_data;
SDL_UnlockMutex (vptr->vid_draw_mutex);       /* done protection */
if (SDL_PushEvent (&user_event) < 0) {
    sim_printf ("%s: vid_draw() SDL_PushEvent error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    free (vid_dst);
    free (vid_data);
    }
}

void vid_draw (int32 x, int32 y, int32 w, int32 h, uint32 *buf)
{
vid_draw_window (&vid_first, x, y, w, h, buf);
}

t_stat vid_set_cursor_window (VID_DISPLAY *vptr, t_bool visible, uint32 width, uint32 height, uint8 *data, uint8 *mask, uint32 hot_x, uint32 hot_y)
{
SDL_Cursor *cursor = SDL_CreateCursor (data, mask, width, height, hot_x, hot_y);
SDL_Event user_event;

sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "vid_set_cursor(%s, %d, %d) Setting New Cursor\n", visible ? "visible" : "invisible", width, height);
if (sim_deb) {
    uint32 i, j;

    for (i=0; i<height; i++) {
        sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "Cursor:  ");
        for (j=0; j<width; j++) {
            int byte = (j + i*width) >> 3;
            int bit = 7 - ((j + i*width) & 0x7);
            static char mode[] = "TWIB";

            sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "%c", mode[(((data[byte]>>bit)&1)<<1)|((mask[byte]>>bit)&1)]);
            }
        sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "\n");
        }
    }

user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_CURSOR;
user_event.user.data1 = cursor;
user_event.user.data2 = (void *)((size_t)visible);

if (SDL_PushEvent (&user_event) < 0) {
    sim_printf ("%s: vid_set_cursor() SDL_PushEvent error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    SDL_FreeCursor (cursor);
    }

return SCPE_OK;
}

t_stat vid_set_cursor (t_bool visible, uint32 width, uint32 height, uint8 *data, uint8 *mask, uint32 hot_x, uint32 hot_y)
{
return vid_set_cursor_window (&vid_first, visible, width, height, data, mask, hot_x, hot_y);
}

void vid_set_cursor_position_window (VID_DISPLAY *vptr, int32 x, int32 y)
{
int32 x_delta = vid_cursor_x - x;
int32 y_delta = vid_cursor_y - y;

if (vptr->vid_flags & SIM_VID_INPUTCAPTURED)
    return;

if ((x_delta) || (y_delta)) {
    sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "vid_set_cursor_position(%d, %d) - Cursor position changed\n", x, y);
    /* Any queued mouse motion events need to have their relative 
       positions adjusted since they were queued based on different info. */
    if (SDL_SemWait (vid_mouse_events.sem) == 0) {
        int32 i;
        SIM_MOUSE_EVENT *ev;

        for (i=0; i<vid_mouse_events.count; i++) {
            ev = &vid_mouse_events.events[(vid_mouse_events.head + i)%MAX_EVENTS];
            sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "Pending Mouse Motion Event Adjusted from: (%d, %d) to (%d, %d)\n", ev->x_rel, ev->y_rel, ev->x_rel + x_delta, ev->y_rel + y_delta);
            ev->x_rel += x_delta;
            ev->y_rel += y_delta;
            }
        if (SDL_SemPost (vid_mouse_events.sem))
            sim_printf ("%s: vid_set_cursor_position(): SDL_SemPost error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
        }
    else {
        sim_printf ("%s: vid_set_cursor_position(): SDL_SemWait error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
        }
    vid_cursor_x = x;
    vid_cursor_y = y;
    if (vptr->vid_cursor_visible) {
        SDL_Event user_event;

        user_event.type = SDL_USEREVENT;
        user_event.user.windowID = vptr->vid_windowID;
        user_event.user.code = EVENT_WARP;
        user_event.user.data1 = NULL;
        user_event.user.data2 = NULL;

        if (SDL_PushEvent (&user_event) < 0)
            sim_printf ("%s: vid_set_cursor_position() SDL_PushEvent error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
        sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "vid_set_cursor_position() - Warp Queued\n");
        }
    else {
        sim_debug (SIM_VID_DBG_CURSOR, vptr->vid_dev, "vid_set_cursor_position() - Warp Skipped\n");
        }
    }
}

void vid_set_cursor_position (int32 x, int32 y)
{
vid_set_cursor_position_window (&vid_first, x, y);
}

void vid_refresh_window (VID_DISPLAY *vptr)
{
SDL_Event user_event;

sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "vid_refresh() - Queueing Refresh Event\n");

user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_REDRAW;
user_event.user.data1 = NULL;
user_event.user.data2 = NULL;

if (SDL_PushEvent (&user_event) < 0)
    sim_printf ("%s: vid_refresh() SDL_PushEvent error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
}

void vid_refresh (void)
{
vid_refresh_window (&vid_first);
}

int vid_map_key (int key)
{
switch (key) {

    case SDLK_BACKSPACE:
        return SIM_KEY_BACKSPACE;

    case SDLK_TAB:
        return SIM_KEY_TAB;

    case SDLK_RETURN:
        return SIM_KEY_ENTER;

    case SDLK_ESCAPE:
        return SIM_KEY_ESC;

    case SDLK_SPACE:
        return SIM_KEY_SPACE;

    case SDLK_QUOTE:
        return SIM_KEY_SINGLE_QUOTE;

    case SDLK_COMMA:
        return SIM_KEY_COMMA;

    case SDLK_MINUS:
        return SIM_KEY_MINUS;

    case SDLK_PLUS:                   
        return SIM_KEY_KP_ADD;        

    case SDLK_PERIOD:
        return SIM_KEY_PERIOD;

    case SDLK_SLASH:
        return SIM_KEY_SLASH;

    case SDLK_0:
        return SIM_KEY_0;

    case SDLK_1:
        return SIM_KEY_1;

    case SDLK_2:
        return SIM_KEY_2;

    case SDLK_3:
        return SIM_KEY_3;

    case SDLK_4:
        return SIM_KEY_4;

    case SDLK_5:
        return SIM_KEY_5;

    case SDLK_6:
        return SIM_KEY_6;

    case SDLK_7:
        return SIM_KEY_7;

    case SDLK_8:
        return SIM_KEY_8;

    case SDLK_9:
        return SIM_KEY_9;

    case SDLK_SEMICOLON:
        return SIM_KEY_SEMICOLON;

    case SDLK_EQUALS:
        return SIM_KEY_EQUALS;

    case SDLK_LEFTBRACKET:
        return SIM_KEY_LEFT_BRACKET;

    case SDLK_BACKSLASH:
        return SIM_KEY_BACKSLASH;

    case SDLK_RIGHTBRACKET:
        return SIM_KEY_RIGHT_BRACKET;

    case SDLK_BACKQUOTE:
        return SIM_KEY_BACKQUOTE;

    case SDLK_a:
        return SIM_KEY_A;

    case SDLK_b:
        return SIM_KEY_B;

    case SDLK_c:
        return SIM_KEY_C;

    case SDLK_d:
        return SIM_KEY_D;

    case SDLK_e:
        return SIM_KEY_E;

    case SDLK_f:
        return SIM_KEY_F;

    case SDLK_g:
        return SIM_KEY_G;

    case SDLK_h:
        return SIM_KEY_H;

    case SDLK_i:
        return SIM_KEY_I;

    case SDLK_j:
        return SIM_KEY_J;

    case SDLK_k:
        return SIM_KEY_K;

    case SDLK_l:
        return SIM_KEY_L;

    case SDLK_m:
        return SIM_KEY_M;

    case SDLK_n:
        return SIM_KEY_N;

    case SDLK_o:
        return SIM_KEY_O;

    case SDLK_p:
        return SIM_KEY_P;

    case SDLK_q:
        return SIM_KEY_Q;

    case SDLK_r:
        return SIM_KEY_R;

    case SDLK_s:
        return SIM_KEY_S;

    case SDLK_t:
        return SIM_KEY_T;

    case SDLK_u:
        return SIM_KEY_U;

    case SDLK_v:
        return SIM_KEY_V;

    case SDLK_w:
        return SIM_KEY_W;

    case SDLK_x:
        return SIM_KEY_X;

    case SDLK_y:
        return SIM_KEY_Y;

    case SDLK_z:
        return SIM_KEY_Z;

    case SDLK_DELETE:
        return SIM_KEY_DELETE;
    case SDLK_KP_0:
        return SIM_KEY_KP_INSERT;

    case SDLK_KP_1:
        return SIM_KEY_KP_END;

    case SDLK_KP_2:
        return SIM_KEY_KP_DOWN;

    case SDLK_KP_3:
        return SIM_KEY_KP_PAGE_DOWN;

    case SDLK_KP_4:
        return SIM_KEY_KP_LEFT;

    case SDLK_KP_5:
        return SIM_KEY_KP_5;

    case SDLK_KP_6:
        return SIM_KEY_KP_RIGHT;

    case SDLK_KP_7:
        return SIM_KEY_KP_HOME;

    case SDLK_KP_8:
        return SIM_KEY_KP_UP;

    case SDLK_KP_9:
        return SIM_KEY_KP_PAGE_UP;

    case SDLK_KP_PERIOD:
        return SIM_KEY_KP_DELETE;

    case SDLK_KP_DIVIDE:
        return SIM_KEY_KP_DIVIDE;

    case SDLK_KP_MULTIPLY:
        return SIM_KEY_KP_MULTIPLY;

    case SDLK_KP_MINUS:
        return SIM_KEY_KP_SUBTRACT;

    case SDLK_KP_PLUS:
        return SIM_KEY_KP_ADD;

    case SDLK_KP_ENTER:
        return SIM_KEY_KP_ENTER;

    case SDLK_UP:
        return SIM_KEY_UP;

    case SDLK_DOWN:
        return SIM_KEY_DOWN;

    case SDLK_RIGHT:
        return SIM_KEY_RIGHT;

    case SDLK_LEFT:
        return SIM_KEY_LEFT;

    case SDLK_INSERT:
        return SIM_KEY_INSERT;

    case SDLK_HOME:
        return SIM_KEY_HOME;

    case SDLK_END:
        return SIM_KEY_END;

    case SDLK_PAGEUP:
        return SIM_KEY_PAGE_UP;

    case SDLK_PAGEDOWN:
        return SIM_KEY_PAGE_DOWN;

    case SDLK_F1:
        return SIM_KEY_F1;

    case SDLK_F2:
        return SIM_KEY_F2;

    case SDLK_F3:
        return SIM_KEY_F3;

    case SDLK_F4:
        return SIM_KEY_F4;

    case SDLK_F5:
        return SIM_KEY_F5;

    case SDLK_F6:
        return SIM_KEY_F6;

    case SDLK_F7:
        return SIM_KEY_F7;

    case SDLK_F8:
        return SIM_KEY_F8;

    case SDLK_F9:
        return SIM_KEY_F9;

    case SDLK_F10:
        return SIM_KEY_F10;

    case SDLK_F11:
        return SIM_KEY_F11;

    case SDLK_F12:
        return SIM_KEY_F12;

    case SDLK_NUMLOCKCLEAR:
        return SIM_KEY_NUM_LOCK;

    case SDLK_CAPSLOCK:
        return SIM_KEY_CAPS_LOCK;

    case SDLK_SCROLLLOCK:
        return SIM_KEY_SCRL_LOCK;

    case SDLK_RSHIFT:
        return SIM_KEY_SHIFT_R;

    case SDLK_LSHIFT:
        return SIM_KEY_SHIFT_L;

    case SDLK_RCTRL:
        return SIM_KEY_CTRL_R;

    case SDLK_LCTRL:
        return SIM_KEY_CTRL_L;

    case SDLK_RALT:
        return SIM_KEY_ALT_R;

    case SDLK_LALT:
        return SIM_KEY_ALT_L;

    case SDLK_LGUI:
        return SIM_KEY_WIN_L;

    case SDLK_RGUI:
        return SIM_KEY_WIN_R;

    case SDLK_PRINTSCREEN:
        return SIM_KEY_PRINT;

    case SDLK_PAUSE:
        return SIM_KEY_PAUSE;

    case SDLK_MENU:
        return SIM_KEY_MENU;

    default:
        return SIM_KEY_UNKNOWN;
        }
}

void vid_joy_motion (SDL_JoyAxisEvent *event)
{
    int n = sizeof motion_callback / sizeof (VID_GAMEPAD_CALLBACK);
    int i;

    for (i = 0; i < n; i++) {
        if (motion_callback[i]) {
            motion_callback[i](event->which, event->axis, event->value);
            }
        }
}

void vid_joy_button (SDL_JoyButtonEvent *event)
{
    int n = sizeof button_callback / sizeof (VID_GAMEPAD_CALLBACK);
    int i;

    for (i = 0; i < n; i++) {
        if (button_callback[i]) {
            button_callback[i](event->which, event->button, event->state);
            }
        }
}

void vid_controller_motion (SDL_ControllerAxisEvent *event)
{
    SDL_JoyAxisEvent e;
    e.which = event->which;
    e.axis = event->axis;
    e.value = event->value;
    vid_joy_motion (&e);
}

void vid_controller_button (SDL_ControllerButtonEvent *event)
{
    /* SDL_GameControllerFromInstanceID is only available from SDL
       version 2.0.4, so check the version at compile time.  The
       version is also checked at run time. */
#if (SDL_MAJOR_VERSION > 2) || (SDL_MAJOR_VERSION == 2 && \
    (SDL_MINOR_VERSION > 0) || (SDL_PATCHLEVEL >= 4))

    SDL_JoyButtonEvent e;
    SDL_GameControllerButtonBind b;
    SDL_GameController *c;
    SDL_GameControllerButton button = (SDL_GameControllerButton)event->button;

    c = SDL_GameControllerFromInstanceID (event->which);
    b = SDL_GameControllerGetBindForButton (c, button);
    e.which = event->which;
    e.button = b.value.button;
    e.state = event->state;
    vid_joy_button (&e);
#endif
}

void vid_key (SDL_KeyboardEvent *event)
{
SIM_KEY_EVENT ev;
VID_DISPLAY *vptr = vid_get_event_window ((SDL_Event *)event, event->windowID);
if (vptr == NULL)
   return;

if (vptr->vid_mouse_captured) {
    static const Uint8 *KeyStates = NULL;
    static int numkeys;

    if (!KeyStates)
        KeyStates = SDL_GetKeyboardState(&numkeys);
    if ((vptr->vid_flags & SIM_VID_INPUTCAPTURED) && 
        (event->state == SDL_PRESSED) && 
        KeyStates[SDL_SCANCODE_RSHIFT] && 
        (KeyStates[SDL_SCANCODE_LCTRL] || KeyStates[SDL_SCANCODE_RCTRL])) {
        sim_debug (SIM_VID_DBG_KEY, vptr->vid_dev, "vid_key() - Cursor Release\n");
        if (SDL_SetRelativeMouseMode(SDL_FALSE) < 0)    /* release cursor, show cursor */
            sim_printf ("%s: vid_key(): SDL_SetRelativeMouseMode error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
        vptr->vid_mouse_captured = FALSE;
        return;
        }
    }
    
if (!sim_is_running)
    return;
    
if (SDL_SemWait (vid_key_events.sem) == 0) {
    if (vid_key_events.count < MAX_EVENTS) {
        ev.key = vid_map_key (event->keysym.sym);
        ev.dev = vptr->vid_dev;
        ev.vptr = vptr;
        sim_debug (SIM_VID_DBG_KEY, vptr->vid_dev, "Keyboard Event: State: %s, Keysym(scancode,sym): (%d,%d) - %s\n", (event->state == SDL_PRESSED) ? "PRESSED" : "RELEASED", event->keysym.scancode, event->keysym.sym, vid_key_name(ev.key));
        if (event->state == SDL_PRESSED) {
            if (!vptr->vid_key_state[event->keysym.scancode]) {/* Key was not down before */
                vptr->vid_key_state[event->keysym.scancode] = TRUE;
                ev.state = SIM_KEYPRESS_DOWN;
                }
            else
                ev.state = SIM_KEYPRESS_REPEAT;
            }
        else {
            vptr->vid_key_state[event->keysym.scancode] = FALSE;
            ev.state = SIM_KEYPRESS_UP;
            }
        vid_key_events.events[vid_key_events.tail++] = ev;
        vid_key_events.count++;
        if (vid_key_events.tail == MAX_EVENTS)
            vid_key_events.tail = 0;
        }
    else {
        sim_debug (SIM_VID_DBG_KEY, vptr->vid_dev, "Keyboard Event DISCARDED: State: %s, Keysym: Scancode: %d, Keysym: %d\n", (event->state == SDL_PRESSED) ? "PRESSED" : "RELEASED", event->keysym.scancode, event->keysym.sym);
        }
    if (SDL_SemPost (vid_key_events.sem))
        sim_printf ("%s: vid_key(): SDL_SemPost error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    }
}

void vid_mouse_move (SDL_MouseMotionEvent *event)
{
SDL_Event dummy_event;
SDL_MouseMotionEvent *dev = (SDL_MouseMotionEvent *)&dummy_event;
SIM_MOUSE_EVENT ev;
VID_DISPLAY *vptr = vid_get_event_window ((SDL_Event *)event, event->windowID);
if (vptr == NULL)
   return;

if ((!vptr->vid_mouse_captured) && (vptr->vid_flags & SIM_VID_INPUTCAPTURED))
    return;


#if defined(CPANEL)
if (vid_active == FALSE) return; 
#else
if (!sim_is_running)
    return;
#endif    
    
if (!vptr->vid_cursor_visible)
    return;
sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Move Event: pos:(%d,%d) rel:(%d,%d) buttons:(%d,%d,%d)\n", 
           event->x, event->y, event->xrel, event->yrel, (event->state & SDL_BUTTON(SDL_BUTTON_LEFT)) ? 1 : 0, (event->state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) ? 1 : 0, (event->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ? 1 : 0);
while (SDL_PeepEvents (&dummy_event, 1, SDL_GETEVENT, SDL_MOUSEMOTION, SDL_MOUSEMOTION)) {
    /* Coalesce motion activity to avoid thrashing */
    event->xrel += dev->xrel;
    event->yrel += dev->yrel;
    event->x = dev->x;
    event->y = dev->y;
    event->state = dev->state;
    sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Move Event: Additional Event Coalesced:pos:(%d,%d) rel:(%d,%d) buttons:(%d,%d,%d)\n", 
        dev->x, dev->y, dev->xrel, dev->yrel, (dev->state & SDL_BUTTON(SDL_BUTTON_LEFT)) ? 1 : 0, (dev->state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) ? 1 : 0, (dev->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ? 1 : 0);
    };
    
#if defined(CPANEL)
ev.drag_flag=0; 
if (event->state & SDL_BUTTON(SDL_BUTTON_LEFT)) {
    int x,y, xm, ym, x0, y0; 
    // mouse left button pressed while moving the mouse -> drag window
    if (window_drag.ongoing_flag==0) {
        // start cpanel window move by dragging the mouse (if allowed on this window)
        if (vptr->vid_flags & SIM_VID_FLAG_ALLOW_DRAG) {
           window_drag.ongoing_flag=1; // drag initiated, checking first movement is big enough
           SDL_GetGlobalMouseState(&xm, &ym);
           SDL_GetWindowPosition(vptr->vid_window, &x, &y); 
           window_drag.x=xm-x; window_drag.y=ym-y; // dx dy to position the windows from global mouse coord
           window_drag.vptr=vptr; 
        }
    } else if (window_drag.vptr != vptr) {
        // drag ... but window has changed because a mouse hop. Terminate drag
        window_drag.ongoing_flag=3; // terminate drag on this window
    } else if (window_drag.ongoing_flag==3) {
        // drag terminated. Must release left mouse button to initiate another
    } else {
        // drag window
        SDL_GetGlobalMouseState(&xm, &ym);
        x=xm-window_drag.x; y=ym-window_drag.y;
        SDL_GetWindowPosition(window_drag.vptr->vid_window, &x0, &y0); 
        if ((window_drag.ongoing_flag==1) && (abs(x-x0) < 5) && (abs(x-x0) < 5)) {
            // too small first movement of mouse -> ignore it
        } else {
            // enought ample first drag mouse movement
            window_drag.ongoing_flag=2; // regular drag
            SDL_SetWindowPosition(window_drag.vptr->vid_window, x, y); 
            // if tooltip visible, request hiding it because cpanel window has been 
            // moved or resized
            if (tooltip.visible) tooltip.hide_requested=1;     
        }
    }
    if (window_drag.ongoing_flag > 1) {
        ev.drag_flag=1; // drag in progress
    }
} else {
    // mouse left button not pressed, so terminate any ongoing window drag
    if (window_drag.ongoing_flag > 1) {
        ev.drag_flag=2; // signal the current mouse release button terminates the current drag
    }
    window_drag.ongoing_flag=0;  
}
if (event->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) {
    // mouse right button pressed while moving the mouse -> show tooltip
    ShowTooltipAtMousePos(event->x, event->y, vptr);
}

#endif
    
if (SDL_SemWait (vid_mouse_events.sem) == 0) {
    if (!vptr->vid_mouse_captured) {
        event->xrel = (event->x - vid_cursor_x);
        event->yrel = (event->y - vid_cursor_y);
        }
    vid_mouse_b1 = (event->state & SDL_BUTTON(SDL_BUTTON_LEFT)) ? TRUE : FALSE;
    vid_mouse_b2 = (event->state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) ? TRUE : FALSE;
    vid_mouse_b3 = (event->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ? TRUE : FALSE;
    sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Move Event: pos:(%d,%d) rel:(%d,%d) buttons:(%d,%d,%d) - Count: %d vid_cursor:(%d,%d)\n", 
                                            event->x, event->y, event->xrel, event->yrel, (event->state & SDL_BUTTON(SDL_BUTTON_LEFT)) ? 1 : 0, (event->state & SDL_BUTTON(SDL_BUTTON_MIDDLE)) ? 1 : 0, (event->state & SDL_BUTTON(SDL_BUTTON_RIGHT)) ? 1 : 0, vid_mouse_events.count, vid_cursor_x, vid_cursor_y);
    if (vid_mouse_events.count < MAX_EVENTS) {
        SIM_MOUSE_EVENT *tail = &vid_mouse_events.events[(vid_mouse_events.tail+MAX_EVENTS-1)%MAX_EVENTS];

        ev.dev = vptr->vid_dev;
#if defined(CPANEL)
        // possible bug in original sim_video.c: fill ev.vptr so it can be associated with its originating window
        ev.vptr = vptr;
#endif
        ev.x_rel = event->xrel;
        ev.y_rel = event->yrel;
        ev.b1_state = vid_mouse_b1;
        ev.b2_state = vid_mouse_b2;
        ev.b3_state = vid_mouse_b3;
        ev.x_pos = event->x;
        ev.y_pos = event->y;
        if ((vid_mouse_events.count > 0) &&             /* Is there a tail event? */
            (ev.b1_state == tail->b1_state) &&          /* With the same button state? */
            (ev.b2_state == tail->b2_state) && 
            (ev.b3_state == tail->b3_state)) {          /* Merge the motion */
            tail->x_rel += ev.x_rel;
            tail->y_rel += ev.y_rel;
            tail->x_pos = ev.x_pos;
            tail->y_pos = ev.y_pos;
            sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Move Event: Coalesced into pending event: (%d,%d)\n", 
                tail->x_rel, tail->y_rel);
            }
        else {                                          /* Add a new event */
            vid_mouse_events.events[vid_mouse_events.tail++] = ev;
            vid_mouse_events.count++;
            if (vid_mouse_events.tail == MAX_EVENTS)
                vid_mouse_events.tail = 0;
            }
        }
    else {
        sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Move Event Discarded: Count: %d\n", vid_mouse_events.count);
        }
    if (SDL_SemPost (vid_mouse_events.sem))
        sim_printf ("%s: vid_mouse_move(): SDL_SemPost error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    }
}

void vid_mouse_button (SDL_MouseButtonEvent *event)
{
SDL_Event dummy_event;
SIM_MOUSE_EVENT ev;
t_bool state;
VID_DISPLAY *vptr = vid_get_event_window ((SDL_Event *)event, event->windowID);
if (vptr == NULL)
   return;

if ((!vptr->vid_mouse_captured) && (vptr->vid_flags & SIM_VID_INPUTCAPTURED)) {
    if ((event->state == SDL_PRESSED) &&
        (event->button == SDL_BUTTON_LEFT)) {               /* left click and cursor not captured? */
        sim_debug (SIM_VID_DBG_KEY, vptr->vid_dev, "vid_mouse_button() - Cursor Captured\n");
        if (SDL_SetRelativeMouseMode (SDL_TRUE) < 0)        /* lock cursor to window, hide cursor */
            sim_printf ("%s: vid_mouse_button(): SDL_SetRelativeMouseMode error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
        SDL_WarpMouseInWindow (NULL, vptr->vid_width/2, vptr->vid_height/2);/* back to center */
        SDL_PumpEvents ();
        while (SDL_PeepEvents (&dummy_event, 1, SDL_GETEVENT, SDL_MOUSEMOTION, SDL_MOUSEMOTION)) {};
        vptr->vid_mouse_captured = TRUE;
        }
    return;
    }
    
#if defined(CPANEL)
if (vid_active == FALSE) return; 
#else
if (!sim_is_running)
    return;
#endif
    
state = (event->state == SDL_PRESSED) ? TRUE : FALSE;
if (SDL_SemWait (vid_mouse_events.sem) == 0) {
    switch (event->button) {
        case SDL_BUTTON_LEFT:
            vid_mouse_b1 = state;
            break;
        case SDL_BUTTON_MIDDLE:
            vid_mouse_b2 = state;
            break;
        case SDL_BUTTON_RIGHT:
            vid_mouse_b3 = state;
            break;
            }
    sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Button Event: State: %d, Button: %d, (%d,%d)\n", event->state, event->button, event->x, event->y);
    if (vid_mouse_events.count < MAX_EVENTS) {
        ev.dev = vptr->vid_dev;
#if defined(CPANEL)
        // possible bug in original sim_video.c: fill ev.vptr so it can be associated with its originating window
        ev.vptr = vptr;
        ev.drag_flag=0;
        if ((vid_mouse_b1==FALSE) && (window_drag.ongoing_flag)) {
            // mouse left button not pressed, so terminate any ongoing window drag
            if (window_drag.ongoing_flag > 1) {
               ev.drag_flag=2; // signal the current mouse release button terminates the current drag
            }
            window_drag.ongoing_flag=0;  
        }
#endif
        ev.x_rel = 0;
        ev.y_rel = 0;
        ev.x_pos = event->x;
        ev.y_pos = event->y;
        ev.b1_state = vid_mouse_b1;
        ev.b2_state = vid_mouse_b2;
        ev.b3_state = vid_mouse_b3;
        vid_mouse_events.events[vid_mouse_events.tail++] = ev;
        vid_mouse_events.count++;
        if (vid_mouse_events.tail == MAX_EVENTS)
            vid_mouse_events.tail = 0;
        }
    else {
        sim_debug (SIM_VID_DBG_MOUSE, vptr->vid_dev, "Mouse Button Event Discarded: Count: %d\n", vid_mouse_events.count);
        }
    if (SDL_SemPost (vid_mouse_events.sem))
        sim_printf ("%s: Mouse Button Event: SDL_SemPost error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    }
    
#if defined(CPANEL)
    if (vid_mouse_b3) {
        // mouse right button pressed while mouse not moving ->show tooltip window
        ShowTooltipAtMousePos(event->x, event->y, vptr); 
    }
#endif
    
}

t_bool vid_is_fullscreen_window (VID_DISPLAY *vptr)
{
return SDL_GetWindowFlags (vptr->vid_window) & SDL_WINDOW_FULLSCREEN_DESKTOP;
}

t_bool vid_is_fullscreen (void)
{
return vid_is_fullscreen_window (&vid_first);
}

t_stat vid_set_fullscreen_window (VID_DISPLAY *vptr, t_bool flag)
{
if (flag)
    SDL_SetWindowFullscreen (vptr->vid_window, SDL_WINDOW_FULLSCREEN_DESKTOP);
else
    SDL_SetWindowFullscreen (vptr->vid_window, 0);
return SCPE_OK;
}

t_stat vid_set_fullscreen (t_bool flag)
{
return vid_set_fullscreen_window (&vid_first, flag);
}

static void vid_stretch(VID_DISPLAY *vptr, SDL_Rect *r)
{
/* Return in r a rectangle with the same aspect ratio as the video
   buffer but scaled to fit precisely in the output window.  Normally,
   the buffer and the window have the same sizes, but if the window is
   resized, or fullscreen is in effect, they are not. */
int w, h;
SDL_GetRendererOutputSize(vptr->vid_renderer, &w, &h);
if ((double)h / vptr->vid_height < (double)w / vptr->vid_width) {
    r->w = vptr->vid_width * h / vptr->vid_height;
    r->h = h;
    r->x = (w - r->w) / 2;
    r->y = 0;
    }
else {
    r->w = w;
    r->h = vptr->vid_height * w / vptr->vid_width;
    r->x = 0;
    r->y = (h - r->h) / 2;
    }
}

void vid_update (VID_DISPLAY *vptr)
{
SDL_Rect vid_dst;
#if defined(CPANEL)
   // possible bug in original sim_video.c check to avoid updating a closing window
   if (vptr->vid_ready == 0) return; 
   // remove use of strecth to avoid subtle texture changes
   SDL_RenderClear (vptr->vid_renderer); 
   SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, NULL, NULL);
   SDL_RenderPresent (vptr->vid_renderer);
   return; 
#else
vid_stretch(vptr, &vid_dst);
#endif
sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "Video Update Event: \n");
if (sim_deb)
    fflush (sim_deb);
if (vptr->vid_blending)
    SDL_RenderPresent (vptr->vid_renderer);
else {
    if (SDL_RenderClear (vptr->vid_renderer))
        sim_printf ("%s: Video Update Event: SDL_RenderClear error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    if (SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, NULL, &vid_dst))
        sim_printf ("%s: Video Update Event: SDL_RenderCopy error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    SDL_RenderPresent (vptr->vid_renderer);
    }
}

#if defined(CPANEL)

void vid_update_texture_scaled(SDL_Rect T, SDL_Texture * texture, int scale, 
                               uint32 * surface_pixels, int surface_width, int surface_height)
{
  #define get_rr_gg_bb  if (sim_end) {      \
                            rr0 = (col >> 16) & 0xFF; gg0 = (col >> 8) & 0xFF;  bb0 = col & 0xFF; \
                         } else {           \
                            rr0 = (col >> 8) & 0xFF;  gg0 = (col >> 16) & 0xFF; bb0 = (col >> 24) & 0xFF; \
                         }                  \
                         if (frac == 256) { \
                            rr += rr0 << 8; gg += gg0 << 8; bb += bb0 << 8; \
                         } else {           \
                            rr += rr0 * frac; gg += gg0 * frac; bb += bb0 * frac; \
                         }

  uint32 * texture_pixels;   
  int surface_pitch, texture_pitch; 
  int texture_x, texture_y; 
  int x0intA, x0intB, x0fracA, x0fracB; 
  int y0intA, y0intB, y0fracA, y0fracB; 
  int y0frac, frac, x0, y0, p_texture, p_surface; 
  int rr0, gg0, bb0, rr, gg, bb, nPixels; 
  int Sx, Sy, Sx0, D; 
  uint32 col; 

  // will copy rectangle T in texture (at given scale) from surface (at scale 100%)
  // work on windows up to 32Kpixels w or h
  
  SDL_LockTexture(texture, &T, (void **) &texture_pixels, &texture_pitch); 
  texture_pitch=texture_pitch / sizeof(* surface_pixels); // pitch is given in bytes -> convert to pitch in pixels
  surface_pitch = surface_width; 

  // texture_y, texture_x will iterate on texture rectangle to generate
  // for each texture pixel, will get the mean of equivanet surface pixels 
  // the texture pìxel texture_x, texture_y maps with a rectangel in surface
  // at coords x0intA|x0fractA, y0intA|y0fractA -> x0intB|x0fractB, y0intB|y0fractB
  // x0int is the integer part, x0frac is the fractional part (base 256 -> pixel coord 0.5 is 128)
  // to calc x9int|x0frac we use Sx, Sy that is coordinates x 65536
  // this reduces number of multiplations, convert many of them on shifts

  texture_y=T.y; 

  D = 65536 * 100 / scale; 
  Sx0 = T.x * D; 
  Sy  = T.y * D; 

next_texture_y:
  texture_x=T.x;  
  p_texture = texture_pitch * (texture_y - T.y);
  Sx = Sx0; 

next_texture_x:
  rr=gg=bb=0;
  nPixels=0;

  // calc the rectangle in surface than maps with the texture pixel
  x0intA=Sx >> 16; x0fracA=(Sx & 0xFFFF) >> 8; x0fracA=256-x0fracA;
  y0intA=Sy >> 16; y0fracA=(Sy & 0xFFFF) >> 8; y0fracA=256-y0fracA;
  x0intB=(Sx + D) >> 16; x0fracB=((Sx + D) & 0xFFFF) >> 8; 
  y0intB=(Sy + D) >> 16; y0fracB=((Sy + D) & 0xFFFF) >> 8; 

//  x0intA=texture_x * 100 / scale; x0fracA=(texture_x * 256 * 100 / scale) & 0xFF; x0fracA=256-x0fracA;
//  y0intA=texture_y * 100 / scale; y0fracA=(texture_y * 256 * 100 / scale) % 0xFF; y0fracA=256-y0fracA;
//  x0intB=(texture_x + 1) * 100 / scale; x0fracB=((texture_x+1) * 256 * 100 / scale) & 0xFF; 
//  y0intB=(texture_y + 1) * 100 / scale; y0fracB=((texture_y+1) * 256 * 100 / scale) & 0xFF; 

  if (x0intA >= surface_width) {x0intA = surface_width-1; x0fracA = 256; }
  if (x0intB >= surface_width) {x0intB = surface_width-1; x0fracB = 256; }
  if (y0intA >= surface_height) {y0intA = surface_height-1; y0fracA = 256; }
  if (y0intB >= surface_height) {y0intB = surface_height-1; y0fracB = 256; }

  y0=y0intA;   
  y0frac=y0fracA; 

next_texture_xy0:
  x0=x0intA;   
  p_surface = x0 + surface_pitch * y0; 

  frac=(x0fracA * y0frac) >> 8;
  if (frac) {
      // rr0 gg0 bb0 =pixel from surface (x0, y0)
      col=surface_pixels[p_surface++]; 
      get_rr_gg_bb
      nPixels+=frac; 
      x0++;
  }
  frac=y0frac;
  while (x0<x0intB) {
      // rr0 gg0 bb0 =pixel from surface (x0, y0)
      col=surface_pixels[p_surface++]; 
      get_rr_gg_bb
      nPixels+=frac; 
      x0++;     
  }
  frac=(x0fracB * y0frac) >> 8;
  if (frac) {
      // rr0 gg0 bb0 =pixel from surface (x0, y0)
      col=surface_pixels[p_surface++]; 
      get_rr_gg_bb
      nPixels+=frac; 
  }

  // pass on horizontal texture line for y0 finished. proceed to next y0 pass

  y0++;
  if (y0<y0intB) {y0frac=256;  goto next_texture_xy0; }  
  if ((y0==y0intB) && (y0fracB)) {y0frac=y0fracB;  goto next_texture_xy0; }
  
  // scanned surface from x0intA,y0intA to x0intB,y0intB taking care of fractional pixels
  // to get the resulting single pixel to put in texture at texture-x, texture.y
  // now compute the color of this single pixel

  rr=rr / nPixels;
  gg=gg / nPixels;
  bb=bb / nPixels;
  // set pixels rr gg bb at texture T.x T.y 
  texture_pixels[p_texture++] = sim_end ? 
               (0xFF000000 | ((rr & 0xFF) << 16) | ((gg & 0xFF) << 8)  | (bb & 0xFF)) : 
               (0x000000FF | ((rr & 0xFF) << 8)  | ((gg & 0xFF) << 16) | ((bb & 0xFF) << 24));
   
  // proceed to next horizontal texture pixel
  texture_x++;
  if (texture_x < T.x + T.w) {
      Sx += D; 
      goto next_texture_x;
  }
  
  // all passes on horizontal texture line finshed. proceed to next texture line
  texture_y++;
  if (texture_y < T.y + T.h) {
      Sy += D; 
      goto next_texture_y;
  }
  
  SDL_UnlockTexture(texture); 
}

void vid_update_texture_noscale(SDL_Rect T, SDL_Texture * texture, int texture_width, int texture_height,
                                int x_src, int y_src,
                                uint32 * surface_pixels, int surface_width, int surface_height)
{
    // T is the destination rectangle in texture
    // x_src, y_src is the source coord in surface

    uint32 * texture_pixels;   
    int surface_pitch, texture_pitch; 
    uint32 col;
    int x, y, p_texture, p_surface; 

    SDL_LockTexture(texture, &T, (void **) &texture_pixels, &texture_pitch); 
    texture_pitch=texture_pitch / sizeof(* surface_pixels); // pitch is given in bytes -> convert to pitch in pixels
    surface_pitch = surface_width; 
    
    for (y=0; y<T.h; y++) {
        p_surface=x_src + (y_src++) * surface_pitch;   // source pixel in surface
        p_texture=y * texture_pitch; // destination pixel in texture
        for (x=0; x<T.w; x++) {
            col=surface_pixels[p_surface++]; 
            texture_pixels[p_texture++]=col;
        }
    }
    SDL_UnlockTexture(texture); 
}                                


// incremental update based on rectanble list
void vid_update_rectlist (VID_DISPLAY *vptr) 
{
    int n,x,y,p; 
    SDL_Rect rect1, rect2; 
    SDL_Rect T; 
    int scale   = vptr->scale;
    int pitch   = vptr->vid_width*sizeof(*vptr->pixels);
    int xpixels = vptr->vid_width;
    int ypixels = vptr->vid_height;
    rectlist TeRectList; // list of texture rectagles to send to renderer

if ((vptr->RectList == NULL) || (vptr->RectList->Count<1) || (vptr->vid_flags & SIM_VID_FLAG_FULL_TEXTURE_REDRAW)) {
    vptr->vid_flags &= ~SIM_VID_FLAG_FULL_TEXTURE_REDRAW; 
    // if no rectangle list or requested to do so, then update full texture
    if (scale < 100) {
        T.x=T.y=0; T.w = vptr->vid_texture_width; T.h = vptr->vid_texture_height; 
        vid_update_texture_scaled(T, vptr->vid_texture, scale, vptr->pixels, vptr->vid_width, vptr->vid_height);
    } else {
        int pitch=vptr->vid_width*sizeof(*vptr->pixels);
        SDL_UpdateTexture(vptr->vid_texture, NULL, vptr->pixels, pitch);
    }
    // send full texture to renderer
    SDL_RenderClear (vptr->vid_renderer);
    SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, NULL, NULL);
} else {
    // copy only data from surface to texture for the suplied rectangle list
    if (scale < 100) {
        // copy rectagles from surface scaling it to texture 
        TeRectList.Count = vptr->RectList->Count; 
        for (n=0; n<vptr->RectList->Count; n++) {
            if (vptr->RectList->flags[n] & RECTLIST_FLAG_NOSCALE) {
                // copy rectagle from surface to texture, no scaling of size (scalen only position=¡)
                T.x=(x=vptr->RectList->x[n]) * scale / 100;        
                T.y=(y=vptr->RectList->y[n]) * scale / 100; 
                T.w=vptr->RectList->w[n]; 
                T.h=vptr->RectList->h[n]; 
                if (T.x + T.w > vptr->vid_texture_width) T.w = vptr->vid_texture_width - T.x; 
                if (T.y + T.h > vptr->vid_texture_height) T.h = vptr->vid_texture_height - T.y; 
                // uses SDL_LockTexture
                vid_update_texture_noscale(T, vptr->vid_texture, vptr->vid_texture_width, vptr->vid_texture_height,
                                           x,y, vptr->pixels, vptr->vid_width, vptr->vid_height);
            } else {
                // copy rectangle from surface to texture applying scaling
                T.x=vptr->RectList->x[n] * scale / 100;        
                T.y=vptr->RectList->y[n] * scale / 100; 
                T.w=((vptr->RectList->x[n] + vptr->RectList->w[n]) * scale + 99) / 100 - T.x; 
                T.h=((vptr->RectList->y[n] + vptr->RectList->h[n]) * scale + 99) / 100 - T.y; 
                if (T.x + T.w > vptr->vid_texture_width) T.w = vptr->vid_texture_width - T.x; 
                if (T.y + T.h > vptr->vid_texture_height) T.h = vptr->vid_texture_height - T.y; 
                // uses SDL_LockTexture
                vid_update_texture_scaled(T, vptr->vid_texture, scale, vptr->pixels, vptr->vid_width, vptr->vid_height);
            }
            TeRectList.x[n]=T.x; TeRectList.w[n]=T.w; 
            TeRectList.y[n]=T.y; TeRectList.h[n]=T.h; 
        }
        // send updated textures (vram) to renderer (mark vram to be redraw in GUI)
        for (n=0; n<vptr->RectList->Count; n++) {
           T.x=TeRectList.x[n]; T.w=TeRectList.w[n]; 
           T.y=TeRectList.y[n]; T.h=TeRectList.h[n]; 
           SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, &T, &T);
        }
    } else {
        // copy rectangles to texture (no scaling needed, if scale >= 100 then texture has same size as surface)
        for (n=0; n<vptr->RectList->Count; n++) {
            x=vptr->RectList->x[n]; y=vptr->RectList->y[n]; p=x + xpixels * y; 
            rect1.x=x; rect1.y=y; rect1.w=vptr->RectList->w[n]; rect1.h=vptr->RectList->h[n]; 
            SDL_UpdateTexture(vptr->vid_texture, &rect1, &(vptr->pixels[p]), pitch);
        }
        // copy each texture rectangle to renderer aplying scale factor
        for (n=0; n<vptr->RectList->Count; n++) {
            rect1.x=vptr->RectList->x[n]; rect1.y=vptr->RectList->y[n]; 
            rect1.w=vptr->RectList->w[n]; rect1.h=vptr->RectList->h[n]; 
            rect2.x=rect1.x * scale / 100; 
            rect2.y=rect1.y * scale / 100; 
            rect2.w=rect1.w * scale / 100; 
            rect2.h=rect1.h * scale / 100; 
           SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, &rect1, &rect2);
        }
    }
}
SDL_RenderPresent (vptr->vid_renderer);

}

void vid_refresh_ex (VID_DISPLAY *vptr, uint32 * pixels, void * RectList)
{
SDL_Event user_event;

sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "vid_refresh() - Queueing Refresh Event\n");

user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_REDRAW;
user_event.user.data1 = (void *) pixels;
user_event.user.data2 = (void *) RectList; 

vid_refresh_in_progress=1; 
sim_debug_cp ("vid_refresh_ex: vid_refresh_in_progress=1 at %d\n", sim_os_msec());

if (SDL_PushEvent (&user_event) < 0)
    sim_printf ("%s: vid_refresh_ex SDL_PushEvent error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
}


void vid_update_ex (SDL_UserEvent *event, VID_DISPLAY *vptr)
{
    uint32 tm0=0; 

    // if no args, sinal end of update
    if (event->data2 == NULL) {
        vid_refresh_in_progress=0; 
        return; 
    }
    sim_debug_cp ("vid_update_ex: start at %d\n", tm0=sim_os_msec());
    
    vptr->pixels   = (uint32 *) event->data1; // set pixels on window;
    vptr->RectList = (rectlist *) event->data2; // set rectlist on window;

    vid_update_rectlist(vptr); 
    if ((tooltip.visible) && (tooltip.parent_vptr == vptr)) vid_update_tooltip(); 
      
    sim_debug_cp ("vid_update_ex: vid_refresh_in_progress=0, duration %d msec\n", sim_os_msec()-tm0);
}

// tooltip is visible, update its contents to keep it sync with main window
void vid_update_tooltip(void)
{
    int x,y,n,parent_xpixels,parent_ypixels,y0,y1; 
    uint32 c; 
    uint32 * tooltip_parent_surface; 

    if (tooltip.visible == 0) return; // safety
    if (tooltip.surface == NULL) return; //safety
    if (tooltip.parent_vptr == NULL) return; //safety
    if (tooltip.texture == NULL) return; //safety
    if (tooltip.renderer == NULL) return; //safety
    parent_xpixels = tooltip.parent_vptr->vid_width; // get with of window tooltip is tied to
    parent_ypixels = tooltip.parent_vptr->vid_height; // get height of window tooltip is tied to
    tooltip_parent_surface = tooltip.parent_vptr->pixels;
    if (parent_xpixels == 0) return; //safety
    if (tooltip_parent_surface == NULL) return; //safety

    // fill in the tooltip pixels to be shown from last redraw surface  pixels data
    n=TOOLTIP_WIDTH*TOOLTIP_HEIGHT*sizeof(*tooltip.surface);
    memset(tooltip.surface, 0, n); 
    for (y=0;y<TOOLTIP_HEIGHT;y++) {
        if (y+tooltip.y_pos >= parent_ypixels) break; 
        y0=(y+tooltip.y_pos)*parent_xpixels;
        y1=y*TOOLTIP_WIDTH;
        for (x=0;x<TOOLTIP_WIDTH;x++) {
            if (x+tooltip.x_pos >= parent_xpixels) break; 
            c = tooltip_parent_surface[x+tooltip.x_pos + y0];
            tooltip.surface[x+y1]=c; 
        }
    }
    // draw a 3 pixel wide white border 
    for (x=0;x<TOOLTIP_WIDTH;x++) tooltip.surface[x+0*TOOLTIP_WIDTH]=tooltip.surface[x+(TOOLTIP_HEIGHT-1)*TOOLTIP_WIDTH]=0xFFffFFff; 
    for (x=1;x<TOOLTIP_WIDTH-1;x++) tooltip.surface[x+1*TOOLTIP_WIDTH]=tooltip.surface[x+(TOOLTIP_HEIGHT-2)*TOOLTIP_WIDTH]=0xFFffFFff; 
    for (y=0;y<TOOLTIP_HEIGHT;y++) tooltip.surface[0+y*TOOLTIP_WIDTH]=tooltip.surface[(TOOLTIP_WIDTH-1)+y*TOOLTIP_WIDTH]=0xFFffFFff; 
    for (y=1;y<TOOLTIP_HEIGHT-1;y++) tooltip.surface[1+y*TOOLTIP_WIDTH]=tooltip.surface[(TOOLTIP_WIDTH-2)+y*TOOLTIP_WIDTH]=0xFFffFFff; 
    for (y=2;y<TOOLTIP_HEIGHT-2;y++) tooltip.surface[2+y*TOOLTIP_WIDTH]=tooltip.surface[(TOOLTIP_WIDTH-3)+y*TOOLTIP_WIDTH]=0xFFffFFff; 
    
    if (SDL_UpdateTexture(tooltip.texture, NULL, tooltip.surface, TOOLTIP_WIDTH*sizeof(*tooltip.surface)))
        sim_printf ("vid_update_tooltip: SDL_UpdateTexture error: %s\n", SDL_GetError());

    if (SDL_RenderClear (tooltip.renderer))
        sim_printf ("vid_update_tooltip: SDL_RenderClear error: %s\n", SDL_GetError());
     if (SDL_RenderCopy (tooltip.renderer, tooltip.texture, NULL, NULL))
        sim_printf ("vid_update_tooltip: SDL_RenderCopy error: %s\n", SDL_GetError());

     SDL_RenderPresent (tooltip.renderer);
}

void CheckIfHideTooltip(SDL_Event * event)
{
    int hide=0;
    if (tooltip.visible==0) return; 
    if (tooltip.parent_vptr == NULL) return; 
    if (tooltip.window == NULL) return; 
    if (tooltip.hide_requested == 1) {
        // hide tooltip because requested to do so
        hide=1; tooltip.hide_requested=0;
    } else if (((SDL_GetWindowFlags(((VID_DISPLAY *) tooltip.parent_vptr)->vid_window) & SDL_WINDOW_INPUT_FOCUS)==0) && 
               ((SDL_GetWindowFlags(tooltip.window) & SDL_WINDOW_INPUT_FOCUS)==0)) {
        // hide tooltip because another window (not tooltip itselt, nor tooltip parent) has the focus
        hide=1; 
    } else if ((event->type==SDL_WINDOWEVENT) && 
        (event->window.event == SDL_WINDOWEVENT_ENTER) && 
        (event->window.windowID==SDL_GetWindowID (tooltip.window))) {
        // hide tooltip because mouse over the tooltip window
        hide=1; 
    } else if ((event->type==SDL_WINDOWEVENT) && 
        ((event->window.event == SDL_WINDOWEVENT_SIZE_CHANGED) || 
         (event->window.event == SDL_WINDOWEVENT_RESIZED) || 
         (event->window.event == SDL_WINDOWEVENT_MINIMIZED) || 
         (event->window.event == SDL_WINDOWEVENT_RESTORED) || 
         (event->window.event == SDL_WINDOWEVENT_HIDDEN) || 
         (event->window.event == SDL_WINDOWEVENT_MOVED)  )&& 
        ((event->window.windowID==((VID_DISPLAY *) tooltip.parent_vptr)->vid_windowID) || 
         (event->window.windowID==SDL_GetWindowID (tooltip.window)))) {
        // hide tooltip because tooltip window/parent window has been moved/resized/hidden
        hide=1; 
    }

    if (hide) {
        SDL_HideWindow(tooltip.window);
        tooltip.visible=0;       
    }
}

// show tooltip popup window that holds the contents of vptr parent window
// under the mouse at 100% scale
// if tooltip window not created, create it
void ShowTooltipAtMousePos(int x_pos, int y_pos, VID_DISPLAY * vptr)
{
    int x,y,w,h,n; 

    // set parent window to take pixels from to fill tooltip contents
    if (vptr==NULL) return; 
    if (tooltip.parent_vptr!=vptr) tooltip.parent_vptr = vptr; 
    // check if tooltips allowed in parent window
    if ((vptr->vid_flags & SIM_VID_FLAG_ALLOW_TOOLTIP) == 0) {
        // parent window does not allows tooltip
        tooltip.parent_vptr=NULL; 
        if (tooltip.visible) {
            if (tooltip.window) SDL_HideWindow(tooltip.window);
            tooltip.visible=0;
        }
        return; 
    }

    // if tooltip windows does not exists, create it
    if (tooltip.window == NULL) {
        SDL_CreateWindowAndRenderer (TOOLTIP_WIDTH, TOOLTIP_HEIGHT, 
            SDL_WINDOW_BORDERLESS | SDL_WINDOW_ALWAYS_ON_TOP, 
            &tooltip.window, &tooltip.renderer);
        if ((tooltip.window == NULL) || (tooltip.renderer == NULL)) {
           sim_printf ("Error Creating Tooltip Video Window: %s\n", SDL_GetError());
           SDL_Quit ();
           return;
        }
        SDL_SetRenderDrawColor (tooltip.renderer, 0, 0, 0, 255);
        tooltip.texture = SDL_CreateTexture (tooltip.renderer,
                                 SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING,
                                 TOOLTIP_WIDTH, TOOLTIP_HEIGHT);
        if (tooltip.texture == NULL) {
            sim_printf ("Error Creating Tooltip Texture: %s\n", SDL_GetError());
            SDL_Quit ();
            return;
        }
        n=TOOLTIP_WIDTH*TOOLTIP_HEIGHT*sizeof(*tooltip.surface);
        tooltip.surface = (uint32 *)malloc(n);
        if (tooltip.surface == NULL) {
            sim_printf ("Error Creating Tooltip Surface\n");
            SDL_Quit ();
            return;
        }
    }

    // x_pos, y_pos is the position of mouse, relative to cpanel window scalled. Convert to unscaled position
    // relative to last redraw surface (the tooltip image will be taken from this). The pos of
    // mouse will be the center of tooltip image
    SDL_GetWindowSize(vptr->vid_window, &w, &h); 
    if (w==0) w=1; if (h==0) h=1; // safety
    tooltip.x_pos = vptr->vid_width * x_pos / w - TOOLTIP_WIDTH / 2; if (tooltip.x_pos < 0) tooltip.x_pos=0;
    tooltip.y_pos = vptr->vid_height * y_pos / h - TOOLTIP_HEIGHT / 2; if (tooltip.y_pos < 0) tooltip.y_pos=0; 
    // render what you want on that tooltip (SDL_RenderClear, SDL_RenderCopy, SDL_RenderPresent) & hide it with SDL_HideWindow
    tooltip.visible=1; 
    tooltip.hide_requested = 0;
    // showing the tooltip at pos relative to main cpanel gui window
    SDL_GetWindowPosition(vptr->vid_window, &x, &y);
    x+=x_pos; y+=y_pos; 
    SDL_SetWindowPosition(tooltip.window, x+15, y+25);
    SDL_ShowWindow(tooltip.window);
    SDL_RaiseWindow(vptr->vid_window);    
    vid_update_tooltip(); 
}

void vid_key_text(SDL_TextInputEvent *event)
{
    VID_DISPLAY *vptr;
    SIM_KEY_EVENT ev;
    int key; 

    if (vid_active == FALSE) return; 
    vptr = vid_get_event_window ((SDL_Event *)event, event->windowID);
    if (vptr == NULL) return;

    // get the typed key (ascii)
    if (event->type==SDL_TEXTINPUT) {
        key=event->text[0]; // get key types
    } else if (event->type==SDL_KEYDOWN) {
        key=((SDL_KeyboardEvent *) (event))->keysym.sym; // add the key sensed in Keydown event
    }
    if (key==0) return; // no key

    if (SDL_SemWait (vid_key_events.sem) == 0) {
        if (vid_key_events.count < MAX_EVENTS) {
            ev.key = key; 
            ev.dev = vptr->vid_dev;
            ev.vptr = vptr;
            ev.state = 0; 
            sim_debug (SIM_VID_DBG_KEY, vptr->vid_dev, "Keyboard key: '%c' (%d)\n", 
                ((key >= 32) && (key<=255)) ? key:0, key);
            vid_key_events.events[vid_key_events.tail++] = ev;
            vid_key_events.count++;
            if (vid_key_events.tail == MAX_EVENTS) vid_key_events.tail = 0;
        }
        if (SDL_SemPost (vid_key_events.sem))
            sim_printf ("%s: vid_key(): SDL_SemPost error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    }
}

void vid_key_ex (SDL_KeyboardEvent *event)
{
    VID_DISPLAY *vptr;
    static const Uint8 *KeyStates = NULL;
    static int numkeys;
    int key; 
    int scale = 0; 

    if (vid_active == FALSE) return; 
    vptr = vid_get_event_window ((SDL_Event *)event, event->windowID);
    if (vptr == NULL) return;

    // set control state global var
    if (!KeyStates) KeyStates = SDL_GetKeyboardState(&numkeys);
    vid_keyb.Cntrl = (KeyStates[SDL_SCANCODE_LCTRL] || KeyStates[SDL_SCANCODE_RCTRL]) ? 1:0;
    vid_keyb.Shift = (KeyStates[SDL_SCANCODE_LSHIFT] || KeyStates[SDL_SCANCODE_RSHIFT]) ? 1:0;
    
    // if key released, clear current keypressed global var and return
    if (event->type==SDL_KEYUP) {
       vid_keyb.KeyPress=0; 
       return;
    }
    
    // key pressed. check keys not sensed in SDL_TEXTINPUT event
    key=0; 
    switch (event->keysym.sym) {
        case SDLK_KP_ENTER:  key=13; break;
        case SDLK_RETURN: case SDLK_BACKSPACE: case SDLK_TAB: case SDLK_ESCAPE: key=event->keysym.sym; 
        default: 
           if ((vid_keyb.Cntrl) && (event->keysym.sym >= 'a') && (event->keysym.sym <= 'z')) {
              // control-A..Z -> ^A..^Z -> ascii codes 1..26
              key = event->keysym.sym - 'a' + 1; 
           }
     }
     // if key>0, it's a key not sensed by SDL_TEXTINPUT. Add it to keybuffer as it was
     // read on SDL_TEXTINPUT event
     if ((key) && 
         ((vptr->vid_flags & SIM_VID_FLAG_ALLOW_TEXTINPUT) || 
          (vptr->vid_flags & SIM_VID_FLAG_ALLOW_ALPHAINPUT)    )) {
         SDL_KeyboardEvent event2; 
         memset(&event2, 0, sizeof(event2)); 
         event2.windowID=event->windowID;
         event2.type=SDL_KEYDOWN; 
         event2.keysym.sym=key; 
         vid_key_text((SDL_TextInputEvent *) &event2);
     }
     // set the current key pressed global var. 
     // Just convert '+' and '-' from keypad as they was the regular '+' and '-' keys
     switch (event->keysym.sym) {
        case SDLK_KP_MINUS:  key='-'; break; 
        case SDLK_KP_PLUS:   key='+'; break; 
     }
     // key has alphabetic chars A..Z stores un lowercase as 'a'..'z'
     // key has the ascii code, but if bit30 is 1 (1<<30) then bits 0..29 holds the scancode
     // KeyPress is set with key pressed right now, set to zero if key released. 
     // LastKeyPressed holds last value set in KeyPress, and is no cleared when key released (can be
     // cleared elsewhere)
     if (key==0) key=event->keysym.sym;
     vid_keyb.KeyPress=key; 
     vid_keyb.LastKeyPress=key; 
     vid_keyb.vptr=vptr; 

    // check if hotkey for scale control active
    if (vptr->vid_flags & SIM_VID_FLAG_SCALE_PLUSMINUS) {
        if (vptr->vid_flags & SIM_VID_FLAG_ALLOW_TEXTINPUT) {
            // textinput -> scale is controlled with 
            // Control Minus -> scale -10%, Control Plus -> scale +10%
            // these two are the only hotkeys for scale control
            if (vid_keyb.Cntrl) {
                if (key == '-') scale=(vptr->scale < 30) ? -5:-10; else
                if (key == '+') scale=(vptr->scale < 30) ? +5:+10; 
            }
        } else {
            // Minus Key -> scale -10%, Plus Key -> scale +10%  (+/-5% id scale <= 30%)
            // Control Minus -> scale -1%, Control Plus -> scale +1%
            if (vid_keyb.Cntrl) {
                if (key == '-') scale=-1; else
                if (key == '+') scale=+1; else
                if (key == 'y'-'a'+1) { // Control-Y (^Y)
                    // toggle scale 50%<->100%
                    if (vptr->scale == 100) scale=-50; else scale=100-vptr->scale; 
                }
            } else if (key == '-') scale=(vptr->scale < 30) ? -5:-10; 
            else if (key == '+') scale=(vptr->scale < 30) ? +5:+10; 
        }
        if (scale) {
            // hotkey to change scale is used
            // calc new scale
            scale = vptr->scale + scale; 
            if (scale>200) scale=200; 
            if (scale<10) scale=10; 
            // has scale changed?
            if (scale != vptr->scale) {
                // yes! resize the GUI window. 
                // New value of vptr->scale  is set inside vid_SetWindowSizeAndPos
                // title of window is set to "(Scale n%)" into vid_SetWindowSizeAndPos
                WindowSizeAndPos.Mode=2; // Mode=2=set size (x=scale 10..200)
                WindowSizeAndPos.x = scale;
                vid_SetWindowSizeAndPos_event (vptr); // resize GUI window            
            }
        }
    }
}


#endif

void vid_update_cursor (VID_DISPLAY *vptr, SDL_Cursor *cursor, int visible)
{
if (!cursor)
    return;
    
#if defined(CPANEL)
    SDL_SetCursor (cursor);
    return;
#endif
    
sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "Cursor Update Event: Previously %s, Now %s, New Cursor object at: %p, Old Cursor object at: %p\n", 
                            SDL_ShowCursor(-1) ? "visible" : "invisible", visible ? "visible" : "invisible", cursor, vptr->vid_cursor);
SDL_SetCursor (cursor);
if ((vptr->vid_window == SDL_GetMouseFocus ()) && visible)
    SDL_WarpMouseInWindow (NULL, vid_cursor_x, vid_cursor_y);/* sync position */
if ((vptr->vid_cursor != cursor) && (vptr->vid_cursor))
    SDL_FreeCursor (vptr->vid_cursor);
vptr->vid_cursor = cursor;
SDL_ShowCursor (visible);
vptr->vid_cursor_visible = visible;
}

void vid_warp_position (VID_DISPLAY *vptr)
{
sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "Mouse Warp Event: Warp to: (%d,%d)\n", vid_cursor_x, vid_cursor_y);

SDL_PumpEvents ();
SDL_WarpMouseInWindow (NULL, vid_cursor_x, vid_cursor_y);
SDL_PumpEvents ();
}

void vid_draw_region (VID_DISPLAY *vptr, SDL_UserEvent *event)
{
SDL_Rect *vid_dst = (SDL_Rect *)event->data1;
uint32 *buf = (uint32 *)event->data2;

sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "Draw Region Event: (%d,%d,%d,%d)\n", vid_dst->x, vid_dst->x, vid_dst->w, vid_dst->h);

SDL_LockMutex (vptr->vid_draw_mutex);
if (vid_dst == vptr->vid_dst_last) {
    vptr->vid_dst_last = NULL;
    vptr->vid_data_last = NULL;
    }
SDL_UnlockMutex (vptr->vid_draw_mutex);

if (vptr->vid_blending) {
    SDL_UpdateTexture(vptr->vid_texture, vid_dst, buf, vid_dst->w*sizeof(*buf));
    SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, vid_dst, vid_dst); 
    }
else
    if (SDL_UpdateTexture(vptr->vid_texture, vid_dst, buf, vid_dst->w*sizeof(*buf)))
        sim_printf ("%s: vid_draw_region() - SDL_UpdateTexture error: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());

free (vid_dst);
free (buf);
event->data1 = NULL;
}

static int vid_new_window (VID_DISPLAY *vptr)
{
    int r; 

#if defined(CPANEL)
    sim_debug_cp("SDL Thread: vid_new_window vptr %x \n", vptr);
    vptr->vid_window = NULL; // init as not done
#endif

    r=SDL_CreateWindowAndRenderer (vptr->vid_width, vptr->vid_height, SDL_WINDOW_SHOWN, 
                                   &vptr->vid_window, &vptr->vid_renderer);
    if (r) vptr->vid_window=NULL; 

if ((vptr->vid_window == NULL) || (vptr->vid_renderer == NULL)) {
    sim_printf ("%s: Error Creating Video Window: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    SDL_Quit ();
    return 0;
    }

vptr->vid_draw_mutex = SDL_CreateMutex();

if (vptr->vid_draw_mutex == NULL) {
    fprintf (stderr, "%s: SDL_CreateMutex failed: %s\n", vid_dname(vptr->vid_dev), SDL_GetError ());
    SDL_Quit ();
    return 0;
    }

SDL_SetRenderDrawColor (vptr->vid_renderer, 0, 0, 0, 255);
SDL_RenderClear (vptr->vid_renderer);
#if defined(CPANEL)
// do not call SDL_RenderPresent (vptr->vid_renderer) now. Doing this would just create
// an ugly black screen flash for no aparent reason. 
#else
SDL_RenderPresent (vptr->vid_renderer);
#endif

vptr->vid_texture = SDL_CreateTexture (vptr->vid_renderer,
                                 SDL_PIXELFORMAT_ARGB8888,
                                 SDL_TEXTUREACCESS_STREAMING,
                                 vptr->vid_width, vptr->vid_height);
if (!vptr->vid_texture) {
    sim_printf ("%s: Error configuring Video environment: %s\n", vid_dname(vptr->vid_dev), SDL_GetError());
    SDL_DestroyRenderer(vptr->vid_renderer);
    vptr->vid_renderer = NULL;
    SDL_DestroyWindow(vptr->vid_window);
    vptr->vid_window = NULL;
    SDL_Quit ();
    return 0;
    }

vptr->vid_format = SDL_AllocFormat (SDL_PIXELFORMAT_ARGB8888);

#if defined(CPANEL)
// allow textinput depending on flag   
if ((vptr->vid_flags & SIM_VID_FLAG_ALLOW_TEXTINPUT) || 
    (vptr->vid_flags & SIM_VID_FLAG_ALLOW_ALPHAINPUT)    ) {
    SDL_StartTextInput();
} else {
    SDL_StopTextInput ();
}
#else
SDL_StopTextInput ();
#endif

vptr->vid_windowID = SDL_GetWindowID (vptr->vid_window);

if (vptr->vid_flags & SIM_VID_INPUTCAPTURED) {
    char title[150];

    memset (title, 0, sizeof(title));
    strlcpy (title, vptr->vid_title, sizeof(title));
    strlcat (title, "                                             ReleaseKey=", sizeof(title));
    strlcat (title, vid_release_key, sizeof(title));
    SDL_SetWindowTitle (vptr->vid_window, title);
    }
else
    SDL_SetWindowTitle (vptr->vid_window, vptr->vid_title);

memset (&vptr->vid_key_state, 0, sizeof(vptr->vid_key_state));
vptr->vid_dst_last = NULL;
vptr->vid_data_last = NULL;

#if defined(CPANEL)
    vptr->vid_texture_width=vptr->vid_width;
    vptr->vid_texture_height=vptr->vid_height;
    vptr->scale=100;
    vptr->pixels=NULL;
    vptr->RectList=NULL;
    if (icon_rgb_defined) vid_set_icon(vptr); // set icon for any new window
    sim_debug_cp("SDL Thread: vid_new_window done vptr %x \n", vptr);
#endif

vid_active++;
return 1;
}

t_stat vid_set_alpha_mode (VID_DISPLAY *vptr, int mode)
{
SDL_BlendMode x;
switch (mode) {
    case SIM_ALPHA_NONE:
        vptr->vid_blending = FALSE;
        x = SDL_BLENDMODE_NONE;
        break;
    case SIM_ALPHA_BLEND:
        vptr->vid_blending = TRUE;
        x = SDL_BLENDMODE_BLEND;
        break;
    case SIM_ALPHA_ADD:
        vptr->vid_blending = TRUE;
        x = SDL_BLENDMODE_ADD;
        break;
    case SIM_ALPHA_MOD:
        vptr->vid_blending = TRUE;
        x = SDL_BLENDMODE_MOD;
        break;
    default:
        return SCPE_ARG;
    }
if (SDL_SetTextureBlendMode (vptr->vid_texture, x))
    return SCPE_IERR;
if (SDL_SetRenderDrawBlendMode (vptr->vid_renderer, x))
    return SCPE_IERR;
return SCPE_OK;
}

static void vid_destroy (VID_DISPLAY *vptr)
{
VID_DISPLAY *parent;
vptr->vid_ready = FALSE;
if (vptr->vid_cursor) {
    SDL_FreeCursor (vptr->vid_cursor);
    vptr->vid_cursor = NULL;
    }
#if defined(CPANEL)
tooltip.visible=0;
if (tooltip.texture)  SDL_DestroyTexture(tooltip.texture);   tooltip.texture  = NULL;
if (tooltip.renderer) SDL_DestroyRenderer(tooltip.renderer); tooltip.renderer = NULL;
if (tooltip.window)   SDL_DestroyWindow(tooltip.window);     tooltip.window   = NULL;
if (tooltip.surface)  free(tooltip.surface);                 tooltip.surface  = NULL; 
#endif
SDL_DestroyTexture(vptr->vid_texture);
vptr->vid_texture = NULL;
SDL_DestroyRenderer(vptr->vid_renderer);
vptr->vid_renderer = NULL;
SDL_DestroyWindow(vptr->vid_window);
vptr->vid_window = NULL;
SDL_DestroyMutex (vptr->vid_draw_mutex);
vptr->vid_draw_mutex = NULL;
for (parent = &vid_first; parent != NULL; parent = parent->next) {
    if (parent->next == vptr)
        parent->next = vptr->next;
    }
vid_active--;
}

int vid_video_events (VID_DISPLAY *vptr0)
{
SDL_Event event;
static const char *eventtypes[SDL_LASTEVENT];
static const char *windoweventtypes[256];
static t_bool initialized = FALSE;

if (!initialized) {
    initialized = TRUE;

    eventtypes[SDL_QUIT] = "QUIT";          /**< User-requested quit */

    /* These application events have special meaning on iOS, see README-ios.txt for details */
    eventtypes[SDL_APP_TERMINATING] = "APP_TERMINATING";   /**< The application is being terminated by the OS
                                     Called on iOS in applicationWillTerminate()
                                     Called on Android in onDestroy()
                                */
    eventtypes[SDL_APP_LOWMEMORY] = "APP_LOWMEMORY";          /**< The application is low on memory, free memory if possible.
                                     Called on iOS in applicationDidReceiveMemoryWarning()
                                     Called on Android in onLowMemory()
                                */
    eventtypes[SDL_APP_WILLENTERBACKGROUND] = "APP_WILLENTERBACKGROUND"; /**< The application is about to enter the background
                                     Called on iOS in applicationWillResignActive()
                                     Called on Android in onPause()
                                */
    eventtypes[SDL_APP_DIDENTERBACKGROUND] = "APP_DIDENTERBACKGROUND"; /**< The application did enter the background and may not get CPU for some time
                                     Called on iOS in applicationDidEnterBackground()
                                     Called on Android in onPause()
                                */
    eventtypes[SDL_APP_WILLENTERFOREGROUND] = "APP_WILLENTERFOREGROUND"; /**< The application is about to enter the foreground
                                     Called on iOS in applicationWillEnterForeground()
                                     Called on Android in onResume()
                                */
    eventtypes[SDL_APP_DIDENTERFOREGROUND] = "APP_DIDENTERFOREGROUND"; /**< The application is now interactive
                                     Called on iOS in applicationDidBecomeActive()
                                     Called on Android in onResume()
                                */

    /* Window events */
    eventtypes[SDL_WINDOWEVENT] = "WINDOWEVENT"; /**< Window state change */
    eventtypes[SDL_SYSWMEVENT] = "SYSWMEVENT";             /**< System specific event */

    windoweventtypes[SDL_WINDOWEVENT_NONE] = "NONE";                /**< Never used */
    windoweventtypes[SDL_WINDOWEVENT_SHOWN] = "SHOWN";              /**< Window has been shown */
    windoweventtypes[SDL_WINDOWEVENT_HIDDEN] = "HIDDEN";            /**< Window has been hidden */
    windoweventtypes[SDL_WINDOWEVENT_EXPOSED] = "EXPOSED";          /**< Window has been exposed and should be
                                                                         redrawn */
    windoweventtypes[SDL_WINDOWEVENT_MOVED] = "MOVED";              /**< Window has been moved to data1, data2
                                     */
    windoweventtypes[SDL_WINDOWEVENT_RESIZED] = "RESIZED";          /**< Window has been resized to data1xdata2 */
    windoweventtypes[SDL_WINDOWEVENT_SIZE_CHANGED] = "SIZE_CHANGED";/**< The window size has changed, either as a result of an API call or through the system or user changing the window size. */
    windoweventtypes[SDL_WINDOWEVENT_MINIMIZED] = "MINIMIZED";      /**< Window has been minimized */
    windoweventtypes[SDL_WINDOWEVENT_MAXIMIZED] = "MAXIMIZED";      /**< Window has been maximized */
    windoweventtypes[SDL_WINDOWEVENT_RESTORED] = "RESTORED";        /**< Window has been restored to normal size
                                                                         and position */
    windoweventtypes[SDL_WINDOWEVENT_ENTER] = "ENTER";              /**< Window has gained mouse focus */
    windoweventtypes[SDL_WINDOWEVENT_LEAVE] = "LEAVE";              /**< Window has lost mouse focus */
    windoweventtypes[SDL_WINDOWEVENT_FOCUS_GAINED] = "FOCUS_GAINED";/**< Window has gained keyboard focus */
    windoweventtypes[SDL_WINDOWEVENT_FOCUS_LOST] = "FOCUS_LOST";    /**< Window has lost keyboard focus */
    windoweventtypes[SDL_WINDOWEVENT_CLOSE] = "CLOSE";              /**< The window manager requests that the
                                                                         window be closed */

    /* Keyboard events */
    eventtypes[SDL_KEYDOWN] = "KEYDOWN";                            /**< Key pressed */
    eventtypes[SDL_KEYUP] = "KEYUP";                                /**< Key released */
    eventtypes[SDL_TEXTEDITING] = "TEXTEDITING";                    /**< Keyboard text editing (composition) */
    eventtypes[SDL_TEXTINPUT] = "TEXTINPUT";                        /**< Keyboard text input */

    /* Mouse events */
    eventtypes[SDL_MOUSEMOTION] = "MOUSEMOTION";                    /**< Mouse moved */
    eventtypes[SDL_MOUSEBUTTONDOWN] = "MOUSEBUTTONDOWN";            /**< Mouse button pressed */
    eventtypes[SDL_MOUSEBUTTONUP] = "MOUSEBUTTONUP";                /**< Mouse button released */
    eventtypes[SDL_MOUSEWHEEL] = "MOUSEWHEEL";                      /**< Mouse wheel motion */

    /* Joystick events */
    eventtypes[SDL_JOYAXISMOTION] = "JOYAXISMOTION";                /**< Joystick axis motion */
    eventtypes[SDL_JOYBALLMOTION] = "JOYBALLMOTION";                /**< Joystick trackball motion */
    eventtypes[SDL_JOYHATMOTION] = "JOYHATMOTION";                  /**< Joystick hat position change */
    eventtypes[SDL_JOYBUTTONDOWN] = "JOYBUTTONDOWN";                /**< Joystick button pressed */
    eventtypes[SDL_JOYBUTTONUP] = "JOYBUTTONUP";                    /**< Joystick button released */
    eventtypes[SDL_JOYDEVICEADDED] = "JOYDEVICEADDED";              /**< A new joystick has been inserted into the system */
    eventtypes[SDL_JOYDEVICEREMOVED] = "JOYDEVICEREMOVED";          /**< An opened joystick has been removed */

    /* Game controller events */
    eventtypes[SDL_CONTROLLERAXISMOTION] = "CONTROLLERAXISMOTION";          /**< Game controller axis motion */
    eventtypes[SDL_CONTROLLERBUTTONDOWN] = "CONTROLLERBUTTONDOWN";          /**< Game controller button pressed */
    eventtypes[SDL_CONTROLLERBUTTONUP] = "CONTROLLERBUTTONUP";              /**< Game controller button released */
    eventtypes[SDL_CONTROLLERDEVICEADDED] = "CONTROLLERDEVICEADDED";        /**< A new Game controller has been inserted into the system */
    eventtypes[SDL_CONTROLLERDEVICEREMOVED] = "CONTROLLERDEVICEREMOVED";    /**< An opened Game controller has been removed */
    eventtypes[SDL_CONTROLLERDEVICEREMAPPED] = "CONTROLLERDEVICEREMAPPED";  /**< The controller mapping was updated */

    /* Touch events */
    eventtypes[SDL_FINGERDOWN] = "FINGERDOWN";
    eventtypes[SDL_FINGERUP] = "FINGERUP";
    eventtypes[SDL_FINGERMOTION] = "FINGERMOTION";

    /* Gesture events */
    eventtypes[SDL_DOLLARGESTURE] = "DOLLARGESTURE";
    eventtypes[SDL_DOLLARRECORD] = "DOLLARRECORD";
    eventtypes[SDL_MULTIGESTURE] = "MULTIGESTURE";

    /* Clipboard events */
    eventtypes[SDL_CLIPBOARDUPDATE] = "CLIPBOARDUPDATE"; /**< The clipboard changed */

    /* Drag and drop events */
    eventtypes[SDL_DROPFILE] = "DROPFILE"; /**< The system requests a file open */

#if (SDL_MINOR_VERSION > 0) || (SDL_PATCHLEVEL >= 3)
    /* Render events */
    eventtypes[SDL_RENDER_TARGETS_RESET] = "RENDER_TARGETS_RESET"; /**< The render targets have been reset */
#endif

#if (SDL_MINOR_VERSION > 0) || (SDL_PATCHLEVEL >= 4)
    /* Render events */
    eventtypes[SDL_RENDER_DEVICE_RESET] = "RENDER_DEVICE_RESET"; /**< The render device has been reset */
#endif

    /** Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
     *  and should be allocated with SDL_RegisterEvents()
     */
    eventtypes[SDL_USEREVENT] = "USEREVENT";
    }

sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE, vptr0->vid_dev, "vid_thread() - Starting\n");

sim_os_set_thread_priority (PRIORITY_ABOVE_NORMAL);

if (!vid_new_window (vptr0)) {
    return 0;
    }
vid_beep_setup (400, 660);
vid_controllers_setup (vptr0->vid_dev);

#if defined(CPANEL)    
    // This hint allows window to receive mouse events when clicked to gain focus. 
    // if not used, windows must be clicked at first, focus is gained when mouse releases, the 
    // the windows can be dragged
    { 
        VID_DISPLAY *vptr = vptr0;
        sim_debug_cp("SDL Thread: vid_video_events vptr0 %x \n", vptr0);
    }
    
    SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, "1");
    memset(&WindowSizeAndPos, 0, sizeof(WindowSizeAndPos)); 

    // init tooltip window to be created on first invocation
    tooltip.window=NULL; 
    tooltip.renderer=NULL; 
    tooltip.texture=NULL; 
    tooltip.visible=0;
    tooltip.surface = NULL;
    tooltip.parent_vptr = NULL;

    SDL_RaiseWindow(vptr0->vid_window);
    { 
        VID_DISPLAY *vptr = vptr0;
        sim_debug_cp ("vid_thread() - Started\n");
    }
#endif

sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE|SIM_VID_DBG_CURSOR, vptr0->vid_dev, "vid_thread() - Started\n");

vptr0->vid_ready = TRUE;

while (vid_active) {
#if defined(CPANEL)
    int status;
    VID_DISPLAY *vptr;
    vptr = vptr0;

    sim_debug_cp ("vid_thread: vid_active %d, call SDL_WaitEvent\n", vid_active);

    status = SDL_WaitEvent (&event); 

    if (0) {
        if (event.type==SDL_WINDOWEVENT) {
           printf("vid_thread: SDL_WaitEvent: status %d, WINDOWSEVENT, %s (event.window.code %d)\n", 
                       status, windoweventtypes[event.window.event], event.window.event);
        } else if (event.type==SDL_USEREVENT) {
           printf("vid_thread: SDL_WaitEvent: status %d, USEREVENT, %s (event.user.code %d)\n", 
                       status, (event.user.code < 12) ? usereventtypes[event.user.code]:"???", event.user.code);
        } else {
           printf("vid_thread: SDL_WaitEvent: status %d, %s (event.type %d)\n", 
                       status, eventtypes[event.type], event.type);
        }
    }
#else
    int status = SDL_WaitEvent (&event);
#endif
    
    if (status == 1) {
        VID_DISPLAY *vptr;
#if defined(CPANEL)
        if (tooltip.visible) CheckIfHideTooltip(&event);  
#endif
        switch (event.type) {

            case SDL_KEYDOWN:
            case SDL_KEYUP:
#if defined(CPANEL)
                vid_key_ex (&event.key);
#else
                vid_key (&event.key);
#endif
                break;

            case SDL_MOUSEBUTTONDOWN:
            case SDL_MOUSEBUTTONUP:
                vid_mouse_button (&event.button);
                break;

            case SDL_MOUSEMOTION:
                vid_mouse_move (&event.motion);
                break;

            case SDL_JOYAXISMOTION:
                vid_joy_motion (&event.jaxis);
                break;

            case SDL_JOYBUTTONUP:
            case SDL_JOYBUTTONDOWN:
                vid_joy_button (&event.jbutton);
                break;

            case SDL_CONTROLLERAXISMOTION:
                vid_controller_motion (&event.caxis);
                break;

            case SDL_CONTROLLERBUTTONUP:
            case SDL_CONTROLLERBUTTONDOWN:
                vid_controller_button (&event.cbutton);
                break;

#if defined(CPANEL)
            case SDL_TEXTINPUT:
                vid_key_text (&event.text);
                break; 
            case SDL_DROPFILE:       
                if ((event.drop.type==SDL_DROPFILE) && (event.drop.file)) {
                    int x,y,x0,y0; 
                    strncpy(DropFile_FileName, event.drop.file, sizeof(DropFile_FileName)); // copy dropped file name
                    SDL_free(event.drop.file);    // Free dropped_filedir memory
                    vptr = vid_get_event_window (&event, event.drop.windowID); // get the window the file is being dropped in
                    if (vptr == NULL) {
                        DropFile_FileName[0]=0; 
                        DropFile_vptr = NULL; 
                        break; 
                    }
                    SDL_GetGlobalMouseState(&x,&y);                  // get global pos of mouse
                    SDL_GetWindowPosition(vptr->vid_window, &x0, &y0); // get pos of dropped in window
                    DropFile_vptr = vptr; 
                    DropFile_x_pos=x-x0; DropFile_y_pos=y-y0;  // this is is x,y the position where the file is dropped in vptr's window coordinates
                    adjust_xy_to_current_scale(vptr, &DropFile_x_pos, &DropFile_y_pos); // adjust coord as if scale is 100% 
                }
                break; 
#endif
            case SDL_WINDOWEVENT:
                vptr = vid_get_event_window (&event, event.window.windowID);
                if (vptr != NULL) {
                    sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE|SIM_VID_DBG_CURSOR, vptr->vid_dev, "vid_thread() - Window Event: %d - %s\n", event.window.event, windoweventtypes[event.window.event]);
                    switch (event.window.event) {
                        case SDL_WINDOWEVENT_ENTER:
                             if (vptr->vid_flags & SIM_VID_INPUTCAPTURED)
                                 SDL_WarpMouseInWindow (NULL, vptr->vid_width/2, vptr->vid_height/2);   /* center position */
                            break;
                        case SDL_WINDOWEVENT_EXPOSED:
                            vid_update (vptr);
                            break;
#if defined(CPANEL)
                        case SDL_WINDOWEVENT_CLOSE:
                            if (vid_quit_callback)
                                vid_quit_callback ();
                            break;
#endif
                        }
                    }
                break;

            case SDL_USEREVENT:
                /* There are 9 user events generated */
                /* EVENT_REDRAW to update the display */
                /* EVENT_DRAW   to update a region in the display texture */
                /* EVENT_SHOW   to display the current SDL video capabilities */
                /* EVENT_CURSOR to change the current cursor */
                /* EVENT_WARP   to warp the cursor position */
                /* EVENT_OPEN   to open a new window */
                /* EVENT_CLOSE  to wake up this thread and let */
                /*              it notice vid_active has changed */
                /* EVENT_SCREENSHOT to take a screenshot */
                /* EVENT_BEEP   to emit a beep sound */
                while (vid_active && event.user.code) {
                    vptr = vid_get_event_window (&event, event.user.windowID);
                    if (vptr == NULL)
                        continue;
                    if (event.user.code == EVENT_REDRAW) {
#if defined(CPANEL)
                        if (event.user.data1) vid_update_ex ((SDL_UserEvent*)&event, vptr); 
                        else vid_update (vptr);                        
#else
                        vid_update (vptr);
#endif
                        event.user.code = 0;    /* Mark as done */
if (0)                        while (SDL_PeepEvents (&event, 1, SDL_GETEVENT, SDL_USEREVENT, SDL_USEREVENT)) {
                            if (event.user.code == EVENT_REDRAW) {
                                /* Only do a single video update between waiting for events */
                                sim_debug (SIM_VID_DBG_VIDEO, vptr->vid_dev, "vid_thread() - Ignored extra REDRAW Event\n");
                                event.user.code = 0;    /* Mark as done */
                                continue;
                                }
                            break;
                            }
                        }
                    if (event.user.code == EVENT_CURSOR) {
                        vid_update_cursor (vptr, (SDL_Cursor *)(event.user.data1), (int)(event.user.data2));
                        event.user.data1 = NULL;
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code == EVENT_WARP) {
                        vid_warp_position (vptr);
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code == EVENT_CLOSE) {
                        vid_destroy (vptr);
                        event.user.code = 0;    /* Mark as done */
#if defined(CPANEL)
                        sim_debug_cp ("vid_thread: EVENT_CLOSE set vid_active=0\n");
#endif
                        }
                    if (event.user.code == EVENT_DRAW) {
                        vid_draw_region (vptr, (SDL_UserEvent*)&event);
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code == EVENT_SHOW) {
                        vid_show_video_event ();
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code == EVENT_SCREENSHOT) {
                        vid_screenshot_event ();
                        event.user.code = 0;    /* Mark as done */
                        }
#if defined(CPANEL)
                    if (event.user.code == EVENT_SIZEANDPOS) {
                        vid_SetWindowSizeAndPos_event (vptr);
                        event.user.code = 0;    /* Mark as done */
                        event.user.data1 = NULL;
                        }
#endif
                    if (event.user.code == EVENT_BEEP) {
                        vid_beep_event ();
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code == EVENT_OPEN) {
                        VID_DISPLAY *vptr = (VID_DISPLAY *)event.user.data1;
                        vid_new_window (vptr);
                        vptr->vid_ready = TRUE;
                        event.user.code = 0;    /* Mark as done */
                        }
                    if (event.user.code != 0) {
                        sim_printf ("vid_thread(): Unexpected user event code: %d\n", event.user.code);
                        }
                    }
                break;
            case SDL_QUIT:
                sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE|SIM_VID_DBG_CURSOR, vptr0->vid_dev, "vid_thread() - QUIT Event - %s\n", vid_quit_callback ? "Signaled" : "Ignored");
                if (vid_quit_callback)
                    vid_quit_callback ();
                break;

            default:
                sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE|SIM_VID_DBG_CURSOR, vptr0->vid_dev, "vid_thread() - Ignored Event: Type: %s(%d)\n", eventtypes[event.type], event.type);
                break;
            }
        }
    else {
        if (status < 0)
            sim_printf ("%s: vid_thread() - SDL_WaitEvent error: %s\n", vid_dname(vptr0->vid_dev), SDL_GetError());
        }
    }   
vid_controllers_cleanup ();
vid_beep_cleanup ();
sim_debug (SIM_VID_DBG_VIDEO|SIM_VID_DBG_KEY|SIM_VID_DBG_MOUSE|SIM_VID_DBG_CURSOR, vptr0->vid_dev, "vid_thread() - Exiting\n");
return 0;
}

int vid_thread (void *arg)
{
VID_DISPLAY *vptr = (VID_DISPLAY *)arg;
int stat;

SDL_SetHint (SDL_HINT_RENDER_DRIVER, "software");

stat = SDL_Init (SDL_INIT_VIDEO);

if (stat) {
    sim_printf ("SDL Video subsystem can't initialize\n");
    return 0;
    }
vid_video_events (vptr);
SDL_Quit ();
return 0;
}

const char *vid_version(void)
{
static char SDLVersion[160];
SDL_version compiled, running;

SDL_GetVersion(&running);

SDL_VERSION(&compiled);

SDLVersion[sizeof (SDLVersion) - 1] = '\0';
if ((compiled.major == running.major) &&
    (compiled.minor == running.minor) &&
    (compiled.patch == running.patch))
    snprintf(SDLVersion, sizeof (SDLVersion) - 1, "SDL Version %d.%d.%d", 
                        compiled.major, compiled.minor, compiled.patch);
else
    snprintf(SDLVersion, sizeof (SDLVersion) - 1, "SDL Version (Compiled: %d.%d.%d, Runtime: %d.%d.%d)", 
                        compiled.major, compiled.minor, compiled.patch,
                        running.major, running.minor, running.patch);
#if defined (HAVE_LIBPNG)
if (1) {
    png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

    if (strcmp (PNG_LIBPNG_VER_STRING, png_get_libpng_ver (png)))
        snprintf(&SDLVersion[strlen (SDLVersion)], sizeof (SDLVersion) - (strlen (SDLVersion) + 1), 
                            ", PNG Version (Compiled: %s, Runtime: %s)", 
                            PNG_LIBPNG_VER_STRING, png_get_libpng_ver (png));
    else
        snprintf(&SDLVersion[strlen (SDLVersion)], sizeof (SDLVersion) - (strlen (SDLVersion) + 1), 
                            ", PNG Version %s", PNG_LIBPNG_VER_STRING);
    png_destroy_read_struct(&png, NULL, NULL);
#if defined (ZLIB_VERSION)
    if (strcmp (ZLIB_VERSION, zlibVersion ()))
        snprintf(&SDLVersion[strlen (SDLVersion)], sizeof (SDLVersion) - (strlen (SDLVersion) + 1), 
                            ", zlib: (Compiled: %s, Runtime: %s)", ZLIB_VERSION, zlibVersion ());
    else
        snprintf(&SDLVersion[strlen (SDLVersion)], sizeof (SDLVersion) - (strlen (SDLVersion) + 1), 
                            ", zlib: %s", ZLIB_VERSION);
#endif
    }
#endif
return (const char *)SDLVersion;
}

t_stat vid_set_release_key (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
return SCPE_NOFNC;
}

t_stat vid_show_release_key (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
VID_DISPLAY *vptr;
for (vptr = &vid_first; vptr != NULL; vptr = vptr->next) {
    if (vptr->vid_flags & SIM_VID_INPUTCAPTURED) {
        fprintf (st, "ReleaseKey=%s", vid_release_key);
        return SCPE_OK;
        }
    }
return SCPE_OK;
}

static t_stat _vid_show_video (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
int i;
VID_DISPLAY *vptr;

fprintf (st, "Video support using SDL: %s\n", vid_version());
#if defined (SDL_MAIN_AVAILABLE)
fprintf (st, "  SDL Events being processed on the main process thread\n");
#endif
if (!vid_active) {
#if !defined (SDL_MAIN_AVAILABLE)
    int stat = SDL_Init(SDL_INIT_VIDEO);

    if (stat)
        return sim_messagef (SCPE_OPENERR, "SDL_Init() failed.  Video subsystem is unavailable.\n");
#endif
    }
else {
    for (vptr = &vid_first; vptr != NULL; vptr = vptr->next) {
        if (!vptr->vid_active_window)
            continue;
        fprintf (st, "  Currently Active Video Window: (%d by %d pixels)\n", vptr->vid_width, vptr->vid_height);
        fprintf (st, "  ");
        vid_show_release_key (st, uptr, val, desc);
        }
    fprintf (st, "\n");
    fprintf (st, "  SDL Video Driver: %s\n", SDL_GetCurrentVideoDriver());
    }
for (i = 0; i < SDL_GetNumVideoDisplays(); ++i) {
    SDL_DisplayMode display;

    if (SDL_GetCurrentDisplayMode(i, &display)) {
        fprintf (st, "Could not get display mode for video display #%d: %s", i, SDL_GetError());
        }
    else {
        fprintf (st, "  Display %s(#%d): current display mode is %dx%dpx @ %dhz. \n", SDL_GetDisplayName(i), i, display.w, display.h, display.refresh_rate);
        }
    }
fprintf (st, "  Available SDL Renderers:\n");
for (i = 0; i < SDL_GetNumRenderDrivers(); ++i) {
    SDL_RendererInfo info;

    if (SDL_GetRenderDriverInfo (i, &info)) {
        fprintf (st, "Could not get render driver info for driver #%d: %s", i, SDL_GetError());
        }
    else {
        uint32 j, k;
        static struct {uint32 format; const char *name;} PixelFormats[] = {
            {SDL_PIXELFORMAT_INDEX1LSB,     "Index1LSB"},
            {SDL_PIXELFORMAT_INDEX1MSB,     "Index1MSB"},
            {SDL_PIXELFORMAT_INDEX4LSB,     "Index4LSB"},
            {SDL_PIXELFORMAT_INDEX4MSB,     "Index4MSB"},
            {SDL_PIXELFORMAT_INDEX8,        "Index8"},
            {SDL_PIXELFORMAT_RGB332,        "RGB332"},
            {SDL_PIXELFORMAT_RGB444,        "RGB444"},
            {SDL_PIXELFORMAT_RGB555,        "RGB555"},
            {SDL_PIXELFORMAT_BGR555,        "BGR555"},
            {SDL_PIXELFORMAT_ARGB4444,      "ARGB4444"},
            {SDL_PIXELFORMAT_RGBA4444,      "RGBA4444"},
            {SDL_PIXELFORMAT_ABGR4444,      "ABGR4444"},
            {SDL_PIXELFORMAT_BGRA4444,      "BGRA4444"},
            {SDL_PIXELFORMAT_ARGB1555,      "ARGB1555"},
            {SDL_PIXELFORMAT_RGBA5551,      "RGBA5551"},
            {SDL_PIXELFORMAT_ABGR1555,      "ABGR1555"},
            {SDL_PIXELFORMAT_BGRA5551,      "BGRA5551"},
            {SDL_PIXELFORMAT_RGB565,        "RGB565"},
            {SDL_PIXELFORMAT_BGR565,        "BGR565"},
            {SDL_PIXELFORMAT_RGB24,         "RGB24"},
            {SDL_PIXELFORMAT_BGR24,         "BGR24"},
            {SDL_PIXELFORMAT_RGB888,        "RGB888"},
            {SDL_PIXELFORMAT_RGBX8888,      "RGBX8888"},
            {SDL_PIXELFORMAT_BGR888,        "BGR888"},
            {SDL_PIXELFORMAT_BGRX8888,      "BGRX8888"},
            {SDL_PIXELFORMAT_ARGB8888,      "ARGB8888"},
            {SDL_PIXELFORMAT_RGBA8888,      "RGBA8888"},
            {SDL_PIXELFORMAT_ABGR8888,      "ABGR8888"},
            {SDL_PIXELFORMAT_BGRA8888,      "BGRA8888"},
            {SDL_PIXELFORMAT_ARGB2101010,   "ARGB2101010"},
            {SDL_PIXELFORMAT_YV12,          "YV12"},
            {SDL_PIXELFORMAT_IYUV,          "IYUV"},
            {SDL_PIXELFORMAT_YUY2,          "YUY2"},
            {SDL_PIXELFORMAT_UYVY,          "UYVY"},
            {SDL_PIXELFORMAT_YVYU,          "YVYU"},
            {SDL_PIXELFORMAT_UNKNOWN,       "Unknown"}};

        fprintf (st, "     Render #%d - %s\n", i, info.name);
        fprintf (st, "        Flags: 0x%X - ", info.flags);
        if (info.flags & SDL_RENDERER_SOFTWARE)
            fprintf (st, "Software|");
        if (info.flags & SDL_RENDERER_ACCELERATED)
            fprintf (st, "Accelerated|");
        if (info.flags & SDL_RENDERER_PRESENTVSYNC)
            fprintf (st, "PresentVSync|");
        if (info.flags & SDL_RENDERER_TARGETTEXTURE)
            fprintf (st, "TargetTexture|");
        fprintf (st, "\n");
        if ((info.max_texture_height != 0) || (info.max_texture_width != 0))
            fprintf (st, "        Max Texture: %d by %d\n", info.max_texture_height, info.max_texture_width);
        fprintf (st, "        Pixel Formats:\n");
        for (j=0; j<info.num_texture_formats; j++) {
            for (k=0; 1; k++) {
                if (PixelFormats[k].format == info.texture_formats[j]) {
                    fprintf (st, "            %s\n", PixelFormats[k].name);
                    break;
                    }
                if (PixelFormats[k].format == SDL_PIXELFORMAT_UNKNOWN) {
                    fprintf (st, "            %s - 0x%X\n", PixelFormats[k].name, info.texture_formats[j]);
                    break;
                    }
                }
            }
        }
    }
if (vid_active) {
    SDL_RendererInfo info;

    for (vptr = &vid_first; vptr != NULL; vptr = vptr->next) {
        if (vptr->vid_active_window) {
            SDL_GetRendererInfo (vptr->vid_renderer, &info);
            break;
            }
        }
    fprintf (st, "  Currently Active Renderer: %s\n", info.name);
    }
if (1) {
    static const char *hints[] = {
#if defined (SDL_HINT_FRAMEBUFFER_ACCELERATION)
                SDL_HINT_FRAMEBUFFER_ACCELERATION   ,
#endif
#if defined (SDL_HINT_RENDER_DRIVER)
                SDL_HINT_RENDER_DRIVER              ,
#endif
#if defined (SDL_HINT_RENDER_OPENGL_SHADERS)
                SDL_HINT_RENDER_OPENGL_SHADERS      ,
#endif
#if defined (SDL_HINT_RENDER_DIRECT3D_THREADSAFE)
                SDL_HINT_RENDER_DIRECT3D_THREADSAFE ,
#endif
#if defined (SDL_HINT_RENDER_DIRECT3D11_DEBUG)
                SDL_HINT_RENDER_DIRECT3D11_DEBUG    ,
#endif
#if defined (SDL_HINT_RENDER_SCALE_QUALITY)
                SDL_HINT_RENDER_SCALE_QUALITY       ,
#endif
#if defined (SDL_HINT_RENDER_VSYNC)
                SDL_HINT_RENDER_VSYNC               ,
#endif
#if defined (SDL_HINT_VIDEO_ALLOW_SCREENSAVER)
                SDL_HINT_VIDEO_ALLOW_SCREENSAVER    ,
#endif
#if defined (SDL_HINT_VIDEO_X11_XVIDMODE)
                SDL_HINT_VIDEO_X11_XVIDMODE         ,
#endif
#if defined (SDL_HINT_VIDEO_X11_XINERAMA)
                SDL_HINT_VIDEO_X11_XINERAMA         ,
#endif
#if defined (SDL_HINT_VIDEO_X11_XRANDR)
                SDL_HINT_VIDEO_X11_XRANDR           ,
#endif
#if defined (SDL_HINT_GRAB_KEYBOARD)
                SDL_HINT_GRAB_KEYBOARD              ,
#endif
#if defined (SDL_HINT_MOUSE_RELATIVE_MODE_WARP)
                SDL_HINT_MOUSE_RELATIVE_MODE_WARP    ,
#endif
#if defined (SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS)
                SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS   ,
#endif
#if defined (SDL_HINT_IDLE_TIMER_DISABLED)
                SDL_HINT_IDLE_TIMER_DISABLED ,
#endif
#if defined (SDL_HINT_ORIENTATIONS)
                SDL_HINT_ORIENTATIONS ,
#endif
#if defined (SDL_HINT_ACCELEROMETER_AS_JOYSTICK)
                SDL_HINT_ACCELEROMETER_AS_JOYSTICK ,
#endif
#if defined (SDL_HINT_XINPUT_ENABLED)
                SDL_HINT_XINPUT_ENABLED ,
#endif
#if defined (SDL_HINT_GAMECONTROLLERCONFIG)
                SDL_HINT_GAMECONTROLLERCONFIG ,
#endif
#if defined (SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS)
                SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS ,
#endif
#if defined (SDL_HINT_ALLOW_TOPMOST)
                SDL_HINT_ALLOW_TOPMOST ,
#endif
#if defined (SDL_HINT_TIMER_RESOLUTION)
                SDL_HINT_TIMER_RESOLUTION ,
#endif
#if defined (SDL_HINT_VIDEO_HIGHDPI_DISABLED)
                SDL_HINT_VIDEO_HIGHDPI_DISABLED ,
#endif
#if defined (SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK)
                SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK ,
#endif
#if defined (SDL_HINT_VIDEO_WIN_D3DCOMPILER)
                SDL_HINT_VIDEO_WIN_D3DCOMPILER              ,
#endif
#if defined (SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT)
                SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT    ,
#endif
#if defined (SDL_HINT_WINRT_PRIVACY_POLICY_URL)
                SDL_HINT_WINRT_PRIVACY_POLICY_URL ,
#endif
#if defined (SDL_HINT_WINRT_PRIVACY_POLICY_LABEL)
                SDL_HINT_WINRT_PRIVACY_POLICY_LABEL ,
#endif
#if defined (SDL_HINT_WINRT_HANDLE_BACK_BUTTON)
                SDL_HINT_WINRT_HANDLE_BACK_BUTTON ,
#endif
#if defined (SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES)
                SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES,
#endif
                NULL};
    fprintf (st, "  Currently Active SDL Hints:\n");
    for (i=0; hints[i]; i++) {
        if (SDL_GetHint (hints[i]))
            fprintf (st, "      %s = %s\n", hints[i], SDL_GetHint (hints[i]));
        }
    }
#if !defined (SDL_MAIN_AVAILABLE)
if (!vid_active)
    SDL_Quit();
#endif
return SCPE_OK;
}

static t_stat _show_stat;
static FILE *_show_st;
static UNIT *_show_uptr;
static int32 _show_val;
static CONST void *_show_desc;

void vid_show_video_event (void)
{
_show_stat = _vid_show_video (_show_st, _show_uptr, _show_val, _show_desc);
}

t_stat vid_show_video (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
SDL_Event user_event;

_show_stat = -1;
_show_st = st;
_show_uptr = uptr;
_show_val = val;
_show_desc = desc;

user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_SHOW;
user_event.user.data1 = NULL;
user_event.user.data2 = NULL;
#if defined (SDL_MAIN_AVAILABLE)
while (SDL_PushEvent (&user_event) < 0)
    sim_os_ms_sleep (10);
#else
vid_show_video_event ();
#endif
while (_show_stat == -1)
    SDL_Delay (20);
return _show_stat;
}

static t_stat _vid_screenshot (VID_DISPLAY *vptr, const char *filename)
{
int stat;
char *fullname = NULL;

if (!vid_active) {
    sim_printf ("No video display is active\n");
    return SCPE_UDIS | SCPE_NOMESSAGE;
    }
fullname = (char *)malloc (strlen(filename) + 5);
if (!fullname)
    return SCPE_MEM;
if (1) {
    SDL_Surface *sshot = sim_end ? SDL_CreateRGBSurface(0, vptr->vid_width, vptr->vid_height, 32, 0x00ff0000, 0x0000ff00, 0x000000ff, 0xff000000) :
                                   SDL_CreateRGBSurface(0, vptr->vid_width, vptr->vid_height, 32, 0x0000ff00, 0x000ff000, 0xff000000, 0x000000ff) ;
    SDL_RenderReadPixels(vptr->vid_renderer, NULL, SDL_PIXELFORMAT_ARGB8888, sshot->pixels, sshot->pitch);
#if defined(HAVE_LIBPNG)
    if (!match_ext (filename, "bmp")) {
        sprintf (fullname, "%s%s", filename, match_ext (filename, "png") ? "" : ".png");
        stat = SDL_SavePNG(sshot, fullname);
        }
    else {
        sprintf (fullname, "%s", filename);
        stat = SDL_SaveBMP(sshot, fullname);
        }
#else
    sprintf (fullname, "%s%s", filename, match_ext (filename, "bmp") ? "" : ".bmp");
    stat = SDL_SaveBMP(sshot, fullname);
#endif /* defined(HAVE_LIBPNG) */
    SDL_FreeSurface(sshot);
    }
if (stat) {
    sim_printf ("Error saving screenshot to %s: %s\n", fullname, SDL_GetError());
    free (fullname);
    return SCPE_IOERR | SCPE_NOMESSAGE;
    }
else {
    if (!sim_quiet)
        sim_printf ("Screenshot saved to %s\n", fullname);
    free (fullname);
    return SCPE_OK;
    }
}

static t_stat _screenshot_stat;
static const char *_screenshot_filename;

void vid_screenshot_event (void)
{
VID_DISPLAY *vptr;
int i = 0, n;
char *name = (char *)malloc (strlen (_screenshot_filename) + 5);
char *extension = strrchr ((char *)_screenshot_filename, '.');
if (name == NULL) {
    _screenshot_stat = SCPE_NXM;
    return;
    }   
if (extension)
    n = extension - _screenshot_filename;
else {
    n = strlen (_screenshot_filename);
    extension = (char *)"";
    }
strncpy (name, _screenshot_filename, n);
for (vptr = &vid_first; vptr != NULL; vptr = vptr->next) {
    if (vid_active > 1)
        sprintf (name + n, "%d%s", i++, extension);
    else
        sprintf (name + n, "%s", extension);
    _screenshot_stat = _vid_screenshot (vptr, name);
    if (_screenshot_stat != SCPE_OK) {
        free (name);
        return;
        }
    }
free (name);
}

t_stat vid_screenshot (const char *filename)
{
SDL_Event user_event;

_screenshot_stat = -1;
_screenshot_filename = filename;

user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_SCREENSHOT;
user_event.user.data1 = NULL;
user_event.user.data2 = NULL;
#if defined (SDL_MAIN_AVAILABLE)
while (SDL_PushEvent (&user_event) < 0)
    sim_os_ms_sleep (10);
#else
vid_screenshot_event ();
#endif
while (_screenshot_stat == -1)
    SDL_Delay (20);
return _screenshot_stat;
}

#if defined(CPANEL)

void adjust_xy_to_current_scale(VID_DISPLAY * vptr, int * x, int * y)
{
    // x,y are in vptr window current (scaled from 10% to 200%) size coordinates. 
    // unapply scale to get the coordinates relative to window as it was unscaled
    int scale = vptr->scale; 
    if ((scale != 100) && (scale >= 10)) { // if GUI scaled, adjust click position to match the unscaled control position
        *x = (*x) * 100 / scale;
        *y = (*y) * 100 / scale;
    }
}

void vid_set_title(VID_DISPLAY * vptr, char * title)
{
SDL_Event user_event;

if (vptr == NULL) return; 
if ((!vid_active) || (!vptr->vid_window)) return; 

user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_SIZEANDPOS;
user_event.user.data1 = NULL; 
user_event.user.data2 = NULL; 

strncpy(vptr->vid_title, title, sizeof(vptr->vid_title)-1);
WindowSizeAndPos.Mode=4; // 4=set title
WindowSizeAndPos.x = 0;
WindowSizeAndPos.y = 0;
WindowSizeAndPos.InProgress = 1;

#if defined (SDL_MAIN_AVAILABLE)
if (SDL_PushEvent (&user_event) < 0)
    sim_printf ("%s: vid_refresh() SDL_PushEvent error: %s\n", sim_dname(vptr->vid_dev), SDL_GetError());
#else
vid_SetWindowSizeAndPos_event (vptr);
#endif
while (WindowSizeAndPos.InProgress) SDL_Delay (20);

}

void vid_SetWindowSizeAndPos_event (VID_DISPLAY * vptr)
{
    int x,y,w,h,set_x, set_y; 
    int old_scale, new_scale, new_w, new_h; 
    char * cptr;
    char buf[sizeof(vptr->vid_title)];
 
    if (!vid_active) {
        sim_printf ("No video display is active\n");
        return;
    }
    if ((vptr==NULL) || (!vptr->vid_window)) {
        sim_printf ("No window is active\n");
        return;
    }

    if (WindowSizeAndPos.Mode==1) {
        // Set windows position
        sim_debug_cp("SDL_SetWindowPosition x=%d, y=%d \n", WindowSizeAndPos.x, WindowSizeAndPos.y);
        SDL_SetWindowPosition(vptr->vid_window, WindowSizeAndPos.x, WindowSizeAndPos.y);
    } else if (WindowSizeAndPos.Mode==2) {
        // calc new windows size
        new_scale=WindowSizeAndPos.x; // get the new scale value
        new_w=vptr->vid_width * new_scale / 100;
        new_h=vptr->vid_height * new_scale / 100;
        sim_debug_cp("SDL_SetWindowSize Scale %d, w=%d, h=%d \n", new_scale, new_w, new_h);
        // get current windows pos and size
        SDL_GetWindowPosition(vptr->vid_window, &x, &y);
        SDL_GetWindowSize(vptr->vid_window, &w, &h);
        set_x=set_y=0;
        // When changing size, the windows grows/shrink but upper left corner remains at same position
        // it this leads to windows not visible then move the windows position so the 
        // windows grows/shrink but wisible windows corner remains at same position
        if ((x<0) && (x+new_w < 16)) {
            x = (x+w-new_w); set_x=1; 
        }
        if ((y<0) && (y+new_h < 16)) {
            y = (y+h-new_h); set_y=1; 
        }
        if ((set_x) || (set_y)) {
            SDL_SetWindowPosition(vptr->vid_window, x, y);
        }
        // after resize, if scale < 100 create a new texture with new window dimensions
        //               if scale>= 100 texture size will remain at 100%
        old_scale=vptr->scale; 
        vptr->scale = new_scale; // set the new scale value
        if ((old_scale >= 100) && (new_scale >= 100)) {
            // changing size from scale >= 100% to scale >= 100% -> no need to destroy & recreate texture
            // texture remains at a max of 100% of surface scale
        } else {
            // destroy and recreate the texture to change its size
            SDL_DestroyTexture(vptr->vid_texture);
            // texture remains at a max of 100% of surface size
            vptr->vid_texture_width = min(new_w, vptr->vid_width); 
            vptr->vid_texture_height = min(new_h, vptr->vid_height);
            vptr->vid_texture = SDL_CreateTexture (vptr->vid_renderer,
                                     SDL_PIXELFORMAT_ARGB8888,
                                     SDL_TEXTUREACCESS_STREAMING,
                                     vptr->vid_texture_width, vptr->vid_texture_height);
            if (vptr->pixels) {
                // if vptr has pointer to window pixels set, redraw full texture at new size
                vptr->vid_flags |= SIM_VID_FLAG_FULL_TEXTURE_REDRAW; 
                vid_update_rectlist(vptr);
            }
        }
        // at last, apply new windows size
        SDL_SetWindowSize(vptr->vid_window, new_w, new_h);
        // and send window texture to resized window
        SDL_RenderClear (vptr->vid_renderer);
        SDL_RenderCopy (vptr->vid_renderer, vptr->vid_texture, NULL, NULL);
        SDL_RenderPresent (vptr->vid_renderer);
        // now modify the window title to include the current scale
        cptr=strstr(vptr->vid_title, " (Scale ");
        if (cptr) { *cptr=0; } // remove "(Scale nn%) from window title
        // add set "(Scale nn%) in window title
        if (strlen(vptr->vid_title) < sizeof(vptr->vid_title) - 20) { 
            sprintf(buf, "%s (Scale %d%%)", vptr->vid_title, new_scale);
            strncpy(vptr->vid_title, buf, sizeof(vptr->vid_title)); 
        }
        SDL_SetWindowTitle(vptr->vid_window, vptr->vid_title);
    } else if (WindowSizeAndPos.Mode==3) {
        // set position relative to current pos
        int x,y; 
        sim_debug_cp("SDL_SetWindowPositionRelative dx=%d, dy=%d \n", WindowSizeAndPos.x, WindowSizeAndPos.y);
        SDL_GetWindowPosition(vptr->vid_window, &x, &y); 
        x += WindowSizeAndPos.x; y += WindowSizeAndPos.y; 
        SDL_SetWindowPosition(vptr->vid_window, x, y);
    } else if (WindowSizeAndPos.Mode==4) {
        // set windows title
        SDL_SetWindowTitle(vptr->vid_window, vptr->vid_title);
    } else if (WindowSizeAndPos.Mode==5) {
        // raise window
        SDL_RaiseWindow(vptr->vid_window);
    }
    WindowSizeAndPos.InProgress = 0;
    WindowSizeAndPos.Mode=0;

    // if tooltip visible, request hiding it because cpanel window has been 
    // moved or resized
    if (tooltip.visible) tooltip.hide_requested=1; 
}

// Mode: 0=none, 
//       1=set pos, 
//       2=set size (x=scale 10..200), 
//       3=move x,y (eg -3, +4 -> move window 3 pixels left, 4 down)
//       4=(reserved, do not use. It is used to implement set title)
//       5=raise window
t_stat vid_SetWindowSizeAndPos (VID_DISPLAY * vptr, int Mode, int x, int y)
{
SDL_Event user_event;

if ((!vid_active) || (vptr==NULL) || (!vptr->vid_window)) return SCPE_OK; 

user_event.type = SDL_USEREVENT;
user_event.user.windowID = vptr->vid_windowID;
user_event.user.code = EVENT_SIZEANDPOS;
user_event.user.data1 = NULL; 
user_event.user.data2 = NULL; 

WindowSizeAndPos.Mode=Mode; // 0=none, 1=set pos, 2=set size (x=scale 10..200), 3=move x,y (eg -3, +4 -> move window 3 pixels les, 4 down)
WindowSizeAndPos.x = x;
WindowSizeAndPos.y = y;
WindowSizeAndPos.InProgress = 1;

#if defined (SDL_MAIN_AVAILABLE)
if (SDL_PushEvent (&user_event) < 0)
    sim_printf ("%s: vid_refresh() SDL_PushEvent error: %s\n", sim_dname(vptr->vid_dev), SDL_GetError());
#else
vid_SetWindowSizeAndPos_event (vptr);
#endif
while (WindowSizeAndPos.InProgress) SDL_Delay (20);

return SCPE_OK;

}

// if Mode='X' or 'Y' return X/Y pos of window in screen coord. 
// if Mode='S' return scale 10..200
 int vid_GetWindowSizeAndPos (VID_DISPLAY * vptr, char Mode)
 {
     if (Mode=='S') {
         return vptr->scale;
     } else if (Mode=='X') {
        int x,y;
        SDL_GetWindowPosition(vptr->vid_window, &x, &y); 
        return x; 
     } else if (Mode=='Y') {
        int x,y;
        SDL_GetWindowPosition(vptr->vid_window, &x, &y); 
        return y; 
     }
     return 0; 
 }

int   icon_rgb_defined = 0; // flag: when 1 -> icon defined
int16 icon_r[32*32];        // holds icon bitmap in RGB (0..255, -1 for transparent)
int16 icon_g[32*32];
int16 icon_b[32*32];

void vid_set_icon(VID_DISPLAY * vptr)
{
    static uint32 icon[32*32] = {0};
    SDL_Surface *surface; 
    SDL_Rect r;
    int x,y,p;

    if (sim_end) {
        surface = SDL_CreateRGBSurfaceFrom(icon,32,32,32,4*32, 0xFF0000, 0x00FF00, 0x00FF, 0xFF000000 /* alpha channel */);
    } else {
        surface = SDL_CreateRGBSurfaceFrom(icon,32,32,32,4*32, 0xFF00, 0xFF0000, 0xFF000000, 0x000000FF /* alpha channel */);
    }
    SDL_FillRect(surface, NULL, 0x01010101); // set all pixels to transparent
    r.h = r.w = 1;
    for (y=0;y<32;y++) for (x=0;x<32;x++) {
        r.x = x; r.y = y; p = x+y*32;
        if (icon_r[p] < 0) continue; // transparent pixel -> skip
        SDL_FillRect(surface, &r, SDL_MapRGB(surface->format, (uint8) icon_r[p], (uint8) icon_g[p], (uint8) icon_b[p])); // draw pixel
    }

    SDL_SetWindowIcon(vptr->vid_window, surface);

    SDL_FreeSurface(surface);
}

// nCursor=1 -> set arrow cursor, 
//        =2 -> hand cursor, 
//        =3 -> Four pointed arrow pointing north, south, east, and west
//        =4 -> wait
//        =0 -> slashed circle (forbidden)
void vid_set_system_cursor (int nCursor)
{
static int current_system_cursor = -1; 
SDL_Cursor *cursor;
SDL_Event user_event;

if (!vid_active) return; 

if (current_system_cursor == nCursor) return;
current_system_cursor = nCursor;

cursor = SDL_CreateSystemCursor( (nCursor==1) ? SDL_SYSTEM_CURSOR_ARROW : 
                                 (nCursor==2) ? SDL_SYSTEM_CURSOR_HAND : 
                                 (nCursor==3) ? SDL_SYSTEM_CURSOR_SIZEALL : 
                                 (nCursor==4) ? SDL_SYSTEM_CURSOR_WAIT : 
                                 SDL_SYSTEM_CURSOR_NO);

user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_CURSOR;
user_event.user.data1 = cursor;
user_event.user.data2 = NULL;

if (SDL_PushEvent (&user_event) < 0) {
    sim_printf ("vid_set_system_cursor() SDL_PushEvent error: %s\n", SDL_GetError());
    SDL_FreeCursor (cursor);
    }
}
#endif


#include <SDL_audio.h>
#include <math.h>

const int AMPLITUDE = 20000;
const int SAMPLE_FREQUENCY = 11025;
static int16 *vid_beep_data;
static int vid_beep_offset;
static int vid_beep_duration;
static int vid_beep_samples;

static void vid_audio_callback(void *ctx, Uint8 *stream, int length)
{
int i, sum, remnant = ((vid_beep_samples - vid_beep_offset) * sizeof (*vid_beep_data));

if (length > remnant) {
    memset (stream + remnant, 0, length - remnant);
    length = remnant;
    if (remnant == 0) {
        SDL_PauseAudio(1);
        return;
        }
    }
memcpy (stream, &vid_beep_data[vid_beep_offset], length);
for (i=sum=0; i<length; i++)
    sum += stream[i];
vid_beep_offset += length / sizeof(*vid_beep_data);
}

static void vid_beep_setup (int duration_ms, int tone_frequency)
{
if (!vid_beep_data) {
    int i;
    SDL_AudioSpec desiredSpec;

    SDL_InitSubSystem (SDL_INIT_AUDIO);
    memset (&desiredSpec, 0, sizeof(desiredSpec));
    desiredSpec.freq = SAMPLE_FREQUENCY;
    desiredSpec.format = AUDIO_S16SYS;
    desiredSpec.channels = 1;
    desiredSpec.samples = 2048;
    desiredSpec.callback = vid_audio_callback;

    SDL_OpenAudio(&desiredSpec, NULL);

    vid_beep_samples = (int)((SAMPLE_FREQUENCY * duration_ms) / 1000.0);
    vid_beep_duration = duration_ms;
    vid_beep_data = (int16 *)malloc (sizeof(*vid_beep_data) * vid_beep_samples);
    for (i=0; i<vid_beep_samples; i++)
        vid_beep_data[i] = (int16)(AMPLITUDE * sin(((double)(i * M_PI * tone_frequency)) / SAMPLE_FREQUENCY));
    }
}

static void vid_beep_cleanup (void)
{
SDL_CloseAudio();
free (vid_beep_data);
vid_beep_data = NULL;
SDL_QuitSubSystem (SDL_INIT_AUDIO);
}

void vid_beep_event (void)
{
vid_beep_offset = 0;                /* reset to beginning of sample set */
SDL_PauseAudio (0);                 /* Play sound */
}

void vid_beep (void)
{
SDL_Event user_event;

user_event.type = SDL_USEREVENT;
user_event.user.code = EVENT_BEEP;
user_event.user.data1 = NULL;
user_event.user.data2 = NULL;
#if defined (SDL_MAIN_AVAILABLE)
while (SDL_PushEvent (&user_event) < 0)
    sim_os_ms_sleep (10);
vid_beep_event ();
#endif
SDL_Delay (vid_beep_duration + 100);/* Wait for sound to finnish */
}

#else /* !(defined(USE_SIM_VIDEO) && defined(HAVE_LIBSDL)) */
/* Non-implemented versions */

t_stat vid_open (DEVICE *dptr, const char *title, uint32 width, uint32 height, int flags)
{
return SCPE_NOFNC;
}

t_stat vid_close (void)
{
return SCPE_OK;
}

t_stat vid_close_all (void)
{
return SCPE_OK;
}

t_stat vid_poll_kb (SIM_KEY_EVENT *ev)
{
return SCPE_EOF;
}

t_stat vid_poll_mouse (SIM_MOUSE_EVENT *ev)
{
return SCPE_EOF;
}

uint32 vid_map_rgb (uint8 r, uint8 g, uint8 b)
{
return 0;
}

void vid_draw (int32 x, int32 y, int32 w, int32 h, uint32 *buf)
{
return;
}

t_stat vid_set_cursor (t_bool visible, uint32 width, uint32 height, uint8 *data, uint8 *mask, uint32 hot_x, uint32 hot_y)
{
return SCPE_NOFNC;
}

void vid_set_cursor_position (int32 x, int32 y)
{
return;
}

void vid_refresh (void)
{
return;
}

void vid_beep (void)
{
return;
}

const char *vid_version (void)
{
return "No Video Support";
}

t_stat vid_set_release_key (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
return SCPE_NOFNC;
}

t_stat vid_show_release_key (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
fprintf (st, "no release key");
return SCPE_OK;
}

t_stat vid_show_video (FILE* st, UNIT* uptr, int32 val, CONST void* desc)
{
fprintf (st, "video support unavailable\n");
return SCPE_OK;
}

t_stat vid_screenshot (const char *filename)
{
sim_printf ("video support unavailable\n");
return SCPE_NOFNC|SCPE_NOMESSAGE;
}

t_bool vid_is_fullscreen (void)
{
sim_printf ("video support unavailable\n");
return FALSE;
}

t_stat vid_set_fullscreen (t_bool flag)
{
sim_printf ("video support unavailable\n");
return SCPE_OK;
}

t_stat vid_open_window (VID_DISPLAY **vptr, DEVICE *dptr, const char *title, uint32 width, uint32 height, int flags)
{
*vptr = NULL;
return SCPE_NOFNC;
}

t_stat vid_close_window (VID_DISPLAY *vptr)
{
return SCPE_OK;
}

uint32 vid_map_rgb_window (VID_DISPLAY *vptr, uint8 r, uint8 g, uint8 b)
{
return 0;
}

void vid_draw_window (VID_DISPLAY *vptr, int32 x, int32 y, int32 w, int32 h, uint32 *buf)
{
return;
}

void vid_refresh_window (VID_DISPLAY *vptr)
{
return;
}

t_stat vid_set_cursor_window (VID_DISPLAY *vptr, t_bool visible, uint32 width, uint32 height, uint8 *data, uint8 *mask, uint32 hot_x, uint32 hot_y)
{
return SCPE_NOFNC;
}

t_bool vid_is_fullscreen_window (VID_DISPLAY *vptr)
{
sim_printf ("video support unavailable\n");
return FALSE;
}

t_stat vid_set_fullscreen_window (VID_DISPLAY *vptr, t_bool flag)
{
sim_printf ("video support unavailable\n");
return SCPE_OK;
}

void vid_set_cursor_position_window (VID_DISPLAY *vptr, int32 x, int32 y)
{
return;
}

const char *vid_key_name (uint32 key)
{
return "";
}
#endif /* defined(USE_SIM_VIDEO) */
