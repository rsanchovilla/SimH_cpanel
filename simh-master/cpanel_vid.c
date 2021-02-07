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


struct cpvidrec cpvid;   
int xpixels, ypixels;
uint32 *surface = NULL;

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



uint32 * get_surface(int *xp, int *yp) 
{
    // return static file-scope surface pointer for direct manipulation outside this module
    // return static xpixels,ypixels (the surface size in pixels) as wx,wy param 
    // handle with care, do not go outside the surface
    *xp = xpixels; 
    *yp = ypixels; 
    return (void *)surface;
}

void key_to_ascii (SIM_KEY_EVENT *kev)
{
    static t_bool k_ctrl, k_shift, k_alt, k_win;
    int c=0; 

#define MODKEY(L, R, mod)   \
    case L: case R: mod = (kev->state != SIM_KEYPRESS_UP); break;
#define MODIFIER_KEYS       \
    MODKEY(SIM_KEY_ALT_L,    SIM_KEY_ALT_R,      k_alt)     \
    MODKEY(SIM_KEY_CTRL_L,   SIM_KEY_CTRL_R,     k_ctrl)    \
    MODKEY(SIM_KEY_SHIFT_L,  SIM_KEY_SHIFT_R,    k_shift)   \
    MODKEY(SIM_KEY_WIN_L,    SIM_KEY_WIN_R,      k_win)
    
    switch (kev->key) {
        MODIFIER_KEYS
        case SIM_KEY_0: case SIM_KEY_1: case SIM_KEY_2: case SIM_KEY_3: case SIM_KEY_4:
        case SIM_KEY_5: case SIM_KEY_6: case SIM_KEY_7: case SIM_KEY_8: case SIM_KEY_9:
            if (kev->state != SIM_KEYPRESS_UP)
                cpvid.last_char = (char)('0' + (kev->key - SIM_KEY_0)); 
            break;
        case SIM_KEY_A: case SIM_KEY_B: case SIM_KEY_C: case SIM_KEY_D: case SIM_KEY_E:
        case SIM_KEY_F: case SIM_KEY_G: case SIM_KEY_H: case SIM_KEY_I: case SIM_KEY_J:
        case SIM_KEY_K: case SIM_KEY_L: case SIM_KEY_M: case SIM_KEY_N: case SIM_KEY_O:
        case SIM_KEY_P: case SIM_KEY_Q: case SIM_KEY_R: case SIM_KEY_S: case SIM_KEY_T:
        case SIM_KEY_U: case SIM_KEY_V: case SIM_KEY_W: case SIM_KEY_X: case SIM_KEY_Y:
        case SIM_KEY_Z: 
            if (kev->state != SIM_KEYPRESS_UP)
                cpvid.last_char = (char)((kev->key - SIM_KEY_A) + 
                                        (k_ctrl ? 1 : (k_shift ? 'A' : 'a')));
            break;
        case SIM_KEY_MINUS          : c='-'; break;
        case SIM_KEY_EQUALS         : c='='; break;
        case SIM_KEY_LEFT_BRACKET   : c='['; break;
        case SIM_KEY_RIGHT_BRACKET  : c=']'; break;
        case SIM_KEY_SEMICOLON      : c=';'; break;
        case SIM_KEY_SINGLE_QUOTE   : c=0x27; break;
        case SIM_KEY_BACKSLASH      : c=0x5c; break;
        case SIM_KEY_COMMA          : c=','; break;
        case SIM_KEY_PERIOD         : c='.'; break;
        case SIM_KEY_SLASH          : c='/'; break;
        case SIM_KEY_KP_ADD         : c='+'; break;
        case SIM_KEY_KP_SUBTRACT    : c='-'; break;
        case SIM_KEY_KP_ENTER       : c=13; break;
        case SIM_KEY_KP_MULTIPLY    : c='*'; break;
        case SIM_KEY_KP_DIVIDE      : c='/'; break;
        case SIM_KEY_ESC            : c=27; break;
        case SIM_KEY_BACKSPACE      : c=127; break;
        case SIM_KEY_TAB            : c=9; break;
        case SIM_KEY_ENTER          : c=13; break;
        case SIM_KEY_SPACE          : c=32; break;
    }
    if (kev->state != SIM_KEYPRESS_UP) {
        if (c>0) cpvid.last_char = (char)c; // last_char set on key release
    }
    cpvid.kev_key = kev->key;       // key being pressed/released
    cpvid.kev_state = kev->state;   // key state SIM_KEYPRESS_DOWN, SIM_KEYPRESS_UP, SIM_KEYPRESS_REPEAT    2
    cpvid.kev_modifier = (k_ctrl ? 2:0) + (k_shift ? 1:0); 

}

int cpvid_init(const char *name, int xp, int yp, void *dptr)
{
    int ret;
    
    xpixels = xp;
    ypixels = yp;
    surface = (uint32 *)realloc (surface, xpixels*ypixels*sizeof(*surface));
    if (!surface) return 0;
    ret = (0 == vid_open ((DEVICE *)dptr, name, xp, yp, 0));
    return ret;
}

void cpvid_shutdown(void)
{

    extern SDL_Window *vid_window;                                 /* window handle */

    if (vid_active==0) return; 
    vid_close();
    while (vid_window) SDL_Delay (20);
    if (surface) free(surface);
    surface = NULL;
}


int cpvid_sync(int checkredraw) 
{
    SDL_Event events[20];
    int i, count, ndraw;

    if (checkredraw) {
        // check if DRAW/REDRAW events in SQL queue; if so do not redraw
        count = SDL_PeepEvents(&events[0], 20, SDL_PEEKEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT);
        ndraw=0;
        for(i=0;i<count;i++) {             
            if (events[i].type != SDL_USEREVENT) continue;
            if ((events[i].user.code != 1 /*EVENT_REDRAW*/) && (events[i].user.code != 5 /*EVENT_DRAW*/)) continue;
            ndraw++;
        }
        // if draw pendings, then exit without adding an additional redraw event
        if ((ndraw > 0) || (count > 18)) return 0; 
    }

    // these are standard call for SDL. vid_draw malloc and memcpy all the surface data, 
    // refresh copies all the data to sdl_texture. 
    // 
    // vid_draw (0, 0, xpixels, ypixels, surface);
    // vid_refresh ();
    //
    // If cpanel is big (say 3000x1000 pixels), surface will have >10MB. To avoid these big
    // malloc and memcopy, a new surface list refresh is used, to update only changes surface zones

    vid_refresh_ex(surface, xpixels, ypixels);
    return 1; // redraw done
}


void cpvid_poll(void)
{
    SIM_MOUSE_EVENT mev;
    SIM_KEY_EVENT kev;

    if (SCPE_OK == vid_poll_mouse (&mev)) {
        
        cpvid.mouse_b1 = mev.b1_state;
        cpvid.X = mev.x_pos; // position of mouse click/release event
        cpvid.Y = mev.y_pos; 
    }
    if (SCPE_OK == vid_poll_kb (&kev)) {
        key_to_ascii (&kev);
    }

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

  