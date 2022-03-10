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
#include "png.h"
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
        cpvid[ncp].InitialScale=100; // default scale=100%
    }
    cpvid_count=0; // mark as initialized
}

// decompose a surface uint32 to its rr,gg,bb components. return alpha channel
int get_surface_rgb_color(uint32 color, int *r_Color, int *g_Color, int *b_Color)
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

// compose a surface uint32 from rr,gg,bb values
uint32 surface_rgb_color(uint32 r, uint32 g, uint32 b)  /* r,g,b are 8bit! */
{
    uint32 color;

    color = sim_end ? 
               (0xFF000000 | ((r & 0xFF) << 16) | ((g & 0xFF) << 8)  | (b & 0xFF)) : 
               (0x000000FF | ((r & 0xFF) << 8)  | ((g & 0xFF) << 16) | ((b & 0xFF) << 24));
    return color;
}

// compose a surface uint32 from rr,gg,bb values, alpha value
uint32 surface_rgb_color_alpha(uint32 r, uint32 g, uint32 b, uint32 alpha)  /* r,g,b are 8bit! */
{
    uint32 color;

    color = sim_end ? 
               (  ((alpha & 0xFF) << 24) | ((r & 0xFF) << 16) | ((g & 0xFF) << 8)  | (b & 0xFF)) : 
               (  (alpha & 0xFF) | ((r & 0xFF) << 8)  | ((g & 0xFF) << 16) | ((b & 0xFF) << 24));
    return color;
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

// create cpanel window at give cpvid entry. If short_name is empty, "MAIN" is used as default name
// initialize cpvid entry, alloc surface 
int cpvid_init(int ncp, const char *title, int xp, int yp, void *dptr)
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
    // create the window on GUI
    r = vid_open_window (&vptr_cp, (DEVICE *)dptr, title, xp, yp, 
                         SIM_VID_FLAG_SCALE_PLUSMINUS | 
                         ((cpvid[ncp].bTextInput==1) ? SIM_VID_FLAG_ALLOW_TEXTINPUT : 0) |
                         ((cpvid[ncp].bTextInput==2) ? SIM_VID_FLAG_ALLOW_ALPHAINPUT : 0) |
                         SIM_VID_FLAG_ALLOW_TOOLTIP   | 
                         SIM_VID_FLAG_ALLOW_DRAG      );
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
    SDL_Event events[20];
    int i, count, ndraw;

    if (vid_refresh_in_progress) {
       sim_debug (CP_REFRESH, &cp_dev, "GUI Refresh skipped because vid_refresh_in_progress\n");
       return 0;
    }
    // check if DRAW/REDRAW events in SQL queue; if so do not redraw
    count = SDL_PeepEvents(&events[0], 20, SDL_PEEKEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT);
    ndraw=0;
    for(i=0;i<count;i++) {             
        if (events[i].type != SDL_USEREVENT) continue;
        if ((events[i].user.code != 1 /*EVENT_REDRAW*/) && (events[i].user.code != 5 /*EVENT_DRAW*/)) continue;
        ndraw++;
    }
    // if draw pendings, then exit without adding an additional redraw event
    if ((ndraw > 0) || (count > 18)) {
        return 0; 
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
    VID_DISPLAY *vptr_cp_last_refreshed = NULL;
    int ncp; 

    // call refresh for each cpanel window in cpvid array
    for (ncp=0; ncp<cpvid_count; ncp++) {
        if (cpvid[ncp].RectList.Count==0) continue; // nothing to refresh
        vptr_cp = cpvid[ncp].vptr_cp; 
        if (vptr_cp == NULL) continue; 
        surface  = cpvid[ncp].surface;  
        RectList = &cpvid[ncp].RectList; 
        vid_refresh_ex(vptr_cp_last_refreshed=vptr_cp, surface, RectList); 
    }
    // indicates the end of updates on gui. Enable refresh again
    if (vptr_cp_last_refreshed) vid_refresh_ex(vptr_cp_last_refreshed, (void *) 1, NULL); 
   

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

/*
PNG load routine
http://zarb.org/~gc/html/libpng.html

*/

#define PNG_DEBUG 3

// read png file, malloc and populate surface with image 
// return it address or zero if error
// set    WW, HH vith size of image
uint32 * read_png_file(char* file_name, int * WW, int * HH)
{
    FILE *fp;

    png_byte color_type;
    png_byte bit_depth;

    png_structp png_ptr;
    png_infop info_ptr;
    int number_of_passes;
    png_bytep * row_pointers;

    char header[8];    // 8 is the maximum size that can be checked
    png_bytep row; 
    png_bytep px; 

    int H, W, x, y, rr, gg, bb, aa, p;
    uint32 col;
    uint32 *pngsurface;

    *HH = *WW = 0;

    /* open file and test for it being a png */
    fp = sim_fopen(file_name, "rb");
    if (!fp) {
        fprintf(stderr, "PNG file %s could not be opened for reading\n", file_name);
        return 0;
    }

    sim_fread(header, 1, 8, fp);
    if (png_sig_cmp((png_const_bytep) header, 0, 8)) {
        fprintf(stderr, "PNG file %s is not recognized as a PNG file\n", file_name);
        fclose(fp);
        return 0;
    }

    /* initialize stuff */
    png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (!png_ptr) {
        fprintf(stderr, "PNG file %s png_create_read_struct failed\n", file_name);
        fclose(fp);
        return 0;
    }

    info_ptr = png_create_info_struct(png_ptr);
    if (!info_ptr) {
        fprintf(stderr, "PNG file %s png_create_info_struct failed\n", file_name);
        free(png_ptr);
        fclose(fp);
        return 0;
    }
    if (setjmp(png_jmpbuf(png_ptr))) {
        fprintf(stderr, "PNG file %s Error during init_io\n", file_name);
        png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
        fclose(fp);
        return 0;
    }

    png_init_io(png_ptr, fp);
    png_set_sig_bytes(png_ptr, 8);
    png_read_info(png_ptr, info_ptr);

    W = png_get_image_width(png_ptr, info_ptr);
    H = png_get_image_height(png_ptr, info_ptr);
    color_type = png_get_color_type(png_ptr, info_ptr);
    bit_depth = png_get_bit_depth(png_ptr, info_ptr);

    *HH=H, *WW=W;

    // Read any color_type into 8bit depth, RGBA format. 
    // See http://www.libpng.org/pub/png/libpng-manual.txt 
 
    if(bit_depth == 16) png_set_strip_16(png_ptr); 
    if(color_type == PNG_COLOR_TYPE_PALETTE) png_set_palette_to_rgb(png_ptr); 

    // PNG_COLOR_TYPE_GRAY_ALPHA is always 8 or 16bit depth. 
    if(color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) png_set_expand_gray_1_2_4_to_8(png_ptr); 
 
    if(png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) png_set_tRNS_to_alpha(png_ptr); 
 
    // These color_type don't have an alpha channel then fill it with 0xff. 
    if(color_type == PNG_COLOR_TYPE_RGB || color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_PALETTE) {
        png_set_filler(png_ptr, 0xFF, PNG_FILLER_AFTER); 
    }
    if(color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA) png_set_gray_to_rgb(png_ptr); 

    number_of_passes = png_set_interlace_handling(png_ptr);
    png_read_update_info(png_ptr, info_ptr);
    
    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
        fprintf(stderr, "PNG file %s Error during read_image\n", file_name);
        png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
        fclose(fp);
        return 0;
    }
        
    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * H);
    for (y=0; y<H; y++) {
        row_pointers[y] = (png_byte*) malloc(png_get_rowbytes(png_ptr,info_ptr));
    }

    png_read_image(png_ptr, row_pointers);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    fclose(fp);

    // alocate mem for read image surface
    pngsurface = (uint32 *)malloc(W*H*sizeof(*pngsurface));
    if (!pngsurface) {
        fprintf(stderr, "PNG file %s surface malloc error\n", file_name);
        return 0;
    }
    p = 0;

    for(y = 0; y < H; y++) { 
        row = row_pointers[y]; 
        for(x = 0; x < W; x++) { 
            px = &(row[x * 4]); // get pixel at x,y
            rr = px[0]; gg = px[1]; bb = px[2]; aa = px[3]; // extract RGBA, A=Alpha
            col = surface_rgb_color(rr,gg,bb);
            pngsurface[p++] = col;         
        }
    }

    // free png mem
    for (y=0; y<H; y++) {
        free(row_pointers[y]);
    }
    free(row_pointers);

    return pngsurface;
}

// Matches the ifdef(CPANEL) at beginning of file
#endif

  

