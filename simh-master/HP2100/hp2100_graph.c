/* hp2100_graph.c: HP2100 graphics translator simulator

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

   Reference:
   - HP1350A Graphics Translator (01350-90908, Jan-1980)


*/

#include "hp2100_defs.h"

extern DEVICE scope_dev;											/* Scope device */
#define HP1350_DETAIL        0x00000100   
#define sim_debug_gra(...)   sim_debug(HP1350_DETAIL, &scope_dev, __VA_ARGS__)

int HP1351_flag = 0;    // select HP1350 or HP1351 model

#define MAX_MEM     ( HP1351_flag ? 8192:2048)      // size of mem depending on HP1350A/HP1351A being emulated
#define MAX_FILES   ( HP1351_flag ? 64:32)          // max number of files allowed depending on HP1350A/HP1351A being emulated

// these pointers allows to access scope_state.xxx not visible here
// the pointer are init in hp2100_scope.c, on scope_init()
int hp1350_Draw_usec = 0; // Draw_usec = time in microseconds to draw the full 2048/8192 mem locations of HP1350A/HP1351A
int hp1350_nLines = 0;    // nLines, nChars = number of lines and vectors draw each frame
int hp1350_nChars = 0;    // 
int hp1350_nVectors = 0;  // nVectors = nLines + lines needed to draw nChars = total numer of vectors effectively draw on scope

#define CMD_BF    1   // Blank File
#define CMD_BM    2   // Blank Memory
#define CMD_CS    3   // Character Size
#define CMD_EF    4   // Erase File
#define CMD_EM    5   // Erase Memory
#define CMD_EN    6   // Erase Names
#define CMD_EX    7   // Erase auXiliary
#define CMD_FF    8   // Find File
#define CMD_FL    9   // Find Location
#define CMD_NF   10   // Name File
#define CMD_PA   11   // Plot Absolute
#define CMD_PE   12   // Pen Enable
#define CMD_SN   13   // Stop Naming
#define CMD_SX   14   // Stop auXiliary
#define CMD_TX   15   // TeXt
#define CMD_UF   16   // Unblank File
#define CMD_UM   17   // Unblank Memory
#define CMD_WX   18   // Write auXiliary

struct {
   int state;       // input char parsing state
   char cmd1, cmd2; // HP1350 command chars received from cpu
   int cmd;         // command current
   int p, p0;       // command params 
} gra_chrin = {0}; 

struct {
    struct {
        int filenum;
        int x0, y0, x,y; // vector in scope
        int ch,chsize;
        int pen,blanked;
        int Aux; // intensity 0=Max, 14=dimmed, 15=blanked
        int usec; // drawing time
    } mem[8192];
    int blank; // global blank flag
    int WritePointer; 
    int NameFileFlag, NameFileNum; // Name File command active
    int AuxFlag, Aux; // WX command active
    int chsize, pen; 
    int xpen, ypen; // beam position left from last PA/TX command
    int current_mem_loc;        // next mem location to be draw on scope refresh 
} gra_state = {0};

struct {
    char * scmd;                    // command mnemonic
    int ncmd;                       // command number
    int has_params;                 // =1 -> command needs param(s)
    int visible_results;            // =1 -> command makes user-visible changes on vectors drawn on screen
} gra_cmd_table[] = {
    {"BF", CMD_BF, 1,1},  // Blank File
    {"BM", CMD_BM, 0,1},  // Blank Memory
    {"CS", CMD_CS, 1,0},  // Character Size
    {"EF", CMD_EF, 1,1},  // Erase File
    {"EM", CMD_EM, 0,1},  // Erase Memory
    {"EN", CMD_EN, 0,0},  // Erase Names
    {"EX", CMD_EX, 0,1},  // Erase auXiliary
    {"FF", CMD_FF, 1,0},  // Find File
    {"FL", CMD_FL, 1,0},  // Find Location
    {"NF", CMD_NF, 1,0},  // Name File
    {"PA", CMD_PA, 1,1},  // Plot Absolute
    {"PE", CMD_PE, 1,0},  // Pen Enable
    {"SN", CMD_SN, 0,0},  // Stop Naming
    {"SX", CMD_SX, 0,0},  // Stop auXiliary
    {"TX", CMD_TX, 2,1},  // TeXt
    {"UF", CMD_UF, 1,1},  // Unblank File
    {"UM", CMD_UM, 0,1},  // Unblank Memory
    {"WX", CMD_WX, 1,0},  // Write auXiliary
    {NULL}
};

#define     B0      120  // slanted border of chars (big slant)
#define     BB       80  // slanted border of chars
struct {                 // vectors to draw character set. Don't touch unless you know what you are doing
    int16 x,y,pen;       // hp doc has some strange char set allocations. Keep default standard char se, assuming 
} gra_char_vectors[] =   // differences are typos in the doc
    { // x,y coords in range 0..500. (0,0) is in lower left corner
      // vectors for charset taken from HP7210A Plotter driver DRV10 for RTE, with some changes
      // so no lower case chars :-(
    { -1, 11,   0},    {250,0,0},{250,500,1}, // vertical tick mark
    { -1, 12,   0},    {0,250,0},{500,250,1}, // horizontal tick mark
    { -1, 14,   0},    {500,500,1},{0,500,0},{500,0,1}, // x marker symbol
    { -1, 15,   0},    {500,0,1},{500,500,1},{0,500,1},{0,0,1}, // rectangle symbol
    { -1, 19,   0},    {133,166,0},{0,250,1},{133,333,1},{0,250,0},{400,250,1}, // pointer <-
    { -1, 30,   0},    {250,0,0},{500,250,1},{250,500,1},{0,250,1},{250,0,1}, // diamond symbol
    { -1, '!',  0},    {250,140,0},{220,500,1},{280,500,1},{250,140,1},{283,55,0},{283,0,1},
                       {222,0,1},{222,55,1},{283,55,1},
    { -1, '"',  0},    {200,389,0},{266,444,1},{266,500,1},{200,500,1},{200,444,1},{266,444,1},
                       {333,389,0},{400,444,1},{400,500,1},{333,500,1},{333,444,1},{400,444,1},
    { -1, '#',  0},    {106,83,0},{173,417,1},{73,333,0},{440,333,1},{373,417,0},{306,83,1},
                       {406,167,0},{40,167,1},
    { -1, '$',  0},    {116,83,0},{183,28,1},{316,28,1},{383,83,1},{383,194,1},{316,250,1},
                       {183,250,1},{116,305,1},{116,416,1},{183,472,1},{316,472,1},{383,416,1},
                       {250,500,0},{250,0,1},
    { -1, '%',  0},    {400,500,1},{133,389,1},{0,389,1},{0,500,1},{133,500,1},{133,389,1},
                       {267,111,0},{267,0,1},{400,0,1},{400,111,1},{267,111,1},{0,0,1},
    { -1, '&',  0},    {400,111,0},{266,0,1},{66,0,1},{0,55,1},{0,194,1},{266,361,1},{266,416,1},
                       {200,472,1},{133,472,1},{67,416,1},{67,361,1},{400,0,1},
    { -1,  39,  0},    {233,389,0},{300,444,1},{300,500,1},{233,500,1},{233,444,1},{300,444,1},  // single quote
    { -1, '(',  0},    {300,500,0},{200,389,1},{200,111,1},{300,0,1},
    { -1, ')',  0},    {200,0,0}, {300,111,1},{300,389,1},{200,500,1},
    { -1, '*',  0},    {250,139,0},{250,361,1},{150,333,0},{350,166,1},{383,250,0},{117,250,1},
                       {150,166,0},{350,333,1},
    { -1, '+',  0},    {50,250,0},{450,250,1},{250,416,0},{250,83,1},
    { -1, ',',  0},    {100,0,0},{166,55,1},{166,111,1},{100,111,1},{100,55,1},{166,55,1},
    { -1, '-',  0},    {50,250,0},{450,250,1},
    { -1, '.',  0},    {166,0,0},{166,55,1},{100,55,1},{100,0,1},{166,0,1},
    { -1, '/',  0},    {400,500,0},{150,0,1},
    { -1, '0',  1},    {B0,0,0},{500-B0,0,1},{500,B0,1},{500,500-B0,1},{500-B0,500,1},{B0,500,1},{0,500-B0,1},
                       {0,B0,1},{B0,0,1},{0,0,0},{500,500,1},
    { -1, '1',  1},    {250-B0,0,0},{250+B0,0,1},{250,0,0},{250,500,1},{250-B0,500-B0,1},
    { -1, '2',  1},    {0,500-BB,0},{BB,500,1},{500-BB,500,1},{500,500-BB,1},{500,230+BB,1},{500-BB,230,1},
                       {BB,230,1},{0,230-BB,1},{0,0,1},{500,0,1},
    { -1, '3',  1},    {0,500-BB,0},{BB,500,1},{500-BB,500,1},{500,500-BB,1},{500,250+BB,1},{500-BB,250,1},
                       {BB,250,1},{500-BB,250,0},{500,250-BB,1},{500,BB,1},{500-BB,0,1},{BB,0,1},{0,BB,1},
    { -1, '4',  1},    {200,500,0},{0,200,1},{500,200,1},{400,500,0},{400,0,1},
    { -1, '5',  1},    {0,BB,0},{BB,0,1},{500-BB,0,1},{500,BB,1},{500,280-BB,1},{500-BB,280,1},{0,280,1},
                       {0,500,1},{500,500,1},
    { -1, '6',  1},    {0,280,0},{500-BB,280,1},{500,280-BB,1},{500,BB,1},{500-BB,0,1},{BB,0,1},{0,BB,1},
                       {0,500-BB,1},{BB,500,1},{500-BB,500,1},{500,500-BB,1},
    { -1, '7',  1},    {0,500,0},{500,500,1},{500,500-BB,1},{0,0,1},
    { -1, '8',  1},    {BB,260,0},{500-BB,260,1},{500,260+BB,1},{500,500-BB,1},{500-BB,500,1},{BB,500,1},
                       {0,500-BB,1},{0,260+BB,1},{BB,260,1},{0,260-BB,1},{0,BB,1},{BB,0,1},{500-BB,0,1},
                       {500,BB,1},{500,260-BB,1},{500-BB,260,1},
    { -1, '9',  1},    {0,BB,0},{BB,0,1},{500-BB,0,1},{500,BB,1},{500,500-BB,1},{500-BB,500,1},{BB,500,1},
                       {0,500-BB,1},{0,250+BB,1},{BB,250,1},{500,250,1},
    { -1, ':',  0},    {100,222,0},{100,277,1},{166,277,1},{166,222,1},{100,222,1},{100,55,0},
                       {100,111,1},{166,111,1},{166,55,1},{100,55,1},
    { -1, ';',  0},    {100,0,0},{166,55,1},{166,111,1},{100,111,1},{100,55,1},{166,55,1},
                       {166,222,0},{166,277,1},{100,277,1},{100,222,1},{166,222,1},
    { -1, '<',  0},    {363,361,0},{97,250,1},{363,139,1},
    { -1, '=',  0},    {363,333,0},{97,333,1},{97,222,0},{363,222,1},
    { -1, '>',  0},    {97,139,0},{363,250,1},{97,361,1},
    { -1, '?',  0},    {50,361,0},{50,416,1},{150,500,1},{283,500,1},{383,416,1},{383,333,1},
                       {217,194,1},{217,110,1},{250,55,0},{250,0,1},{184,0,1},{184,55,1},
                       {250,55,1},
    { -1, '@',  0},    {366,194,0},{400,361,1},{200,361,1},{100,305,1},{66,194,1},{133,139,1},
                       {333,139,1},{366,194,1},{433,83,1},{500,111,1},{500,417,1},{400,500,1},
                       {100,500,1},{0,417,1},{0,83,1},{100,0,1},{366,0,1},{500,55,1},
    { -1, 'A',  1},    {250,500,1},{500,0,1},{90,166,0},{395,166,1},
    { -1, 'B',  1},    {500-BB,0,1},{500,BB,1},{500,270-BB,1},{500-BB*2,270,1},{450,270+BB,1},{450,500-BB,1},
                       {450-BB,500,1},{0,500,1},{0,0,1},{0,270,1},{500-BB*2,270,1},
    { -1, 'C',  1},    {500,500-BB,0},{500-BB,500,1},{BB,500,1},{0,500-BB,1},{0,BB,1},{BB,0,1},{500-BB,0,1},{500,BB,1},
    { -1, 'D',  1},    {500-BB,0,1},{500,BB,1},{500,500-BB,1},{500-BB,500,1},{0,500,1},{0,0,1},
    { -1, 'E',  1},    {0,500,1},{500,500,1},{0,278,0},{400,278,1},{0,0,0},{500,0,1},
    { -1, 'F',  1},    {0,500,1},{500,500,1},{0,278,0},{400,278,1},
    { -1, 'G',  1},    {500,500-BB,0},{500-BB,500,1},{BB,500,1},{0,500-BB,1},{0,BB,1},{BB,0,1},{500,0,1},
                       {500,166,1},{400,166,1},
    { -1, 'H',  1},    {0,500,1},{0,250,0},{500,250,1},{500,500,0},{500,0,1},
    { -1, 'I',  1},    {250-BB,0,0},{250+BB,0,1},{250,0,0},{250,500,1},{250-BB,500,0},{250+BB,500,1},
    { -1, 'J',  1},    {0,BB,0},{BB,0,1},{300,0,1},{300+BB,BB,1},{300+BB,500,1},{300,500,0},{300+2*BB,500,1},
    { -1, 'K',  1},    {0,500,1},{500,500,0},{0,222,1},{200,333,0},{500,0,1},
    { -1, 'L',  1},    {0,500,0},{0,0,1},{500,0,1},
    { -1, 'M',  0},    {0,500,1},{BB,500,1},{250,83,1},{500-BB,500,1},{500,500,1},{500,0,1},
    { -1, 'N',  1},    {0,500,1},{BB,500,1},{500,0,1},{500,500,1},
    { -1, 'O',  1},    {BB,0,0},{500-BB,0,1},{500,BB,1},{500,500-BB,1},{500-BB,500,1},{BB,500,1},
                       {0,500-BB,1},{0,BB,1},{BB,0,1},
    { -1, 'P',  1},    {0,500,1},{500-BB,500,1},{500,500-BB,1},{500,250+BB,1},{500-BB,250,1},{0,250,1},
    { -1, 'Q',  0},    {BB,0,0},{400-BB,0,1},{400,BB,1},{400,500-BB,1},{400-BB,500,1},{BB,500,1},
                       {0,500-BB,1},{0,BB,1},{BB,0,1},{300,111,0},{500-BB,0,1},{500,0,1},
    { -1, 'R',  1},    {0,500,1},{500-BB,500,1},{500,500-BB,1},{500,250+BB,1},{500-BB,250,1},{500,0,1},
                       {0,250,0},{500-BB,250,1},
    { -1, 'S',  1},    {0,BB,0},{BB,0,1},{500-BB,0,1},{500,BB,1},{500,277-BB,1},{500-BB,277,1},{BB,277,1},
                       {0,277+BB,1},{0,500-BB,1},{BB,500,1},{500-BB,500,1},{500,500-BB,1},
    { -1, 'T',  1},    {250,0,0},{250,500,1},{0,500,0},{500,500,1},
    { -1, 'U',  1},    {0,500,0},{0,BB,1},{BB,0,1},{500-BB,0,1},{500,BB,1},{500,500,1},
    { -1, 'V',  1},    {0,500,0},{210,0,1},{290,0,1},{500,500,1},
    { -1, 'W',  0},    {0,500,0},{120,0,1},{150,0,1},{250,416,1},{350,0,1},{380,0,1},{500,500,1},
    { -1, 'X',  1},    {500,500,1},{0,500,0},{500,0,1},
    { -1, 'Y',  1},    {0,500,0},{250,250,1},{500,500,1},{255,250,0},{250,0,1},
    { -1, 'Z',  1},    {0,500,0},{500,500,1},{0,0,1},{500,0,1},
    { -1, '[',  0},    {300,500,0},{200,500,1},{200,0,1},{300,0,1}, // ascii code = 91
    { -1, 92,   0},    {150,500,0},{400,0,1}, // backslash bar "\"
    { -1, ']',  0},    {200,0,0},{300,0,1},{300,500,1},{200,500,1}, // ascii code = 92
    { -1, 94,   0},    {250,0,0},{250,500,1},{150,389,0},{250,500,1},{350,389,1}, // up arrow (^)
    { -1, '_',  0},    {500,0,1}, // underscore ascii code = 95
    { -1, 96,   0},    {0,250,0},{100,250,1},{170,0,1},{200,0,1},{300,500,1},{500,500,1}, // square root

    { -1, 123,  0},    {100,250,0},{500,250,1},{200,250,0},{100,0,1},{300,0,0},{400,250,1}, // PI symbol
    { -1, 124,  0},    {250,0,0},{250,500,1}, // vertical pipe
    { -1, 125,  0},    {366,166,0},{500,250,1},{366,333,1},{500,250,0},{100,250,1}, // pointer ->
    { -1, 126,  0},    {400,BB,0},{400,0,1},{0,0,1},{0,BB,1},{150,250,1},{0,500-BB,1},{0,500,1},
                       {400,500,1},{400,500-BB,1},// sigma symbol
    { -1, 127,  0},    {100,100,0},{100,400,1},{100,250,0},{400,250,1}, // |- symbol
    { -2}              // end of char vector table
};

void draw_char_vectors(int x_base, int y_base, 
                       char ch, int chsize, 
                       int decay, int intensity, 
                       int * count_nVectors); 

// draw vectors from list that needs drawing_msec to be drawn
void HP1350_draw_vectors(int drawing_msec)
{
    extern void draw_vector_in_scope(int x1_HP, int y1_HP, int x2_HP, int y2_HP, int decay, int intensity); 
    int start, usec, remain_usec, decay_usec; 
    int loc, nLocs, intensity; 
    static int count_nLines, count_nChars, count_nVectors, Total_Draw_usec; 
    int max_mem = MAX_MEM; 

    if (gra_state.blank) return; // display blanked; 

    // calc how many memory locations can be draw during drawing_msec
    start=loc=gra_state.current_mem_loc;  
    remain_usec=drawing_msec * 1000; 
    nLocs=0; // nLocs to draw 
    while(1) {
        usec = gra_state.mem[loc].usec; 
        if ( (gra_state.mem[loc].blanked) || (gra_state.mem[loc].pen==0)) usec = usec / 8; 
        if (usec<2) usec=2; 
        remain_usec -= usec; 
        if (remain_usec < 0) break; // drawing time exhausted
        nLocs++;
        loc++;
        if (loc >= max_mem) loc=0;        
        if (loc == start) break; // all list processed
    }
    // draw vectors calculating the decay each one should have
    loc=gra_state.current_mem_loc;  
    decay_usec = drawing_msec * 1000 - remain_usec;
    while (nLocs--) {
        usec = gra_state.mem[loc].usec; 
        if ( (gra_state.mem[loc].blanked) || (gra_state.mem[loc].pen==0)) {
            // blanked 
            usec = usec / 8; 
        } else if (gra_state.mem[loc].chsize == 0) {
            // is a single vector
            count_nLines++; count_nVectors++;
            intensity=gra_state.mem[loc].Aux; // intensity 0=max intensity, 14=dimmed, 15=not visible
            intensity=intensity+2; // vectors have reduced intensity
            draw_vector_in_scope(gra_state.mem[loc].x0, gra_state.mem[loc].y0, 
                                 gra_state.mem[loc].x,  gra_state.mem[loc].y, 
                                 decay_usec / 1000, intensity); 
        } else {
            // is a char
            count_nChars++;
            intensity=gra_state.mem[loc].Aux; // intensity 0=max intensity, 14=dimmed, 15=not visible
            draw_char_vectors(gra_state.mem[loc].x,  gra_state.mem[loc].y,
                              gra_state.mem[loc].ch, gra_state.mem[loc].chsize, 
                              decay_usec / 1000, intensity, 
                              &count_nVectors); 
        }
        if (usec<2) usec=2; 
        Total_Draw_usec += usec; 
        decay_usec -= usec; if (decay_usec<0) decay_usec=0;

        loc++; 
        if (loc >= max_mem) {
            loc=0;        
            // mem fully scanned. save value of counters to be displayed in ShowInfo panel
            hp1350_Draw_usec=Total_Draw_usec; 
            hp1350_nLines=count_nLines; 
            hp1350_nChars=count_nChars; 
            hp1350_nVectors=count_nVectors; 
            // clear counters for next frame
            Total_Draw_usec=0; 
            count_nLines=count_nChars=count_nVectors=0;
        }

    }
    gra_state.current_mem_loc=loc; 
}

void draw_char_vectors(int x_base, int y_base, 
                       char ch, int chsize, 
                       int decay, int intensity, 
                       int * count_nVectors) 
{
    extern void draw_vector_in_scope(int x1_HP, int y1_HP, int x2_HP, int y2_HP, int decay, int intensity); 
    int ww,hh,rotated; 
    int n, nVect,x,y,x0,y0,x1,y1,c,pen; 
    static int charmap[128] = {0};

    if ((ch<10) || (ch>127)) return; 
    ch=sim_toupper(ch);

    // populate charmap if needed
    if (charmap[0]==0) {
        charmap[0]=1; 
        // locate first vector of char
        for (n=0; gra_char_vectors[n].x != -2; n++) {
            if (gra_char_vectors[n].x != -1) continue;
            c=gra_char_vectors[n].y; // char code
            charmap[c]=n+1; // start of char vectors
            if (gra_char_vectors[n].pen) {
                // scale-down the char 0.9 on x axis
                int m=n+1; 
                while ((x=gra_char_vectors[m].x) >=0) {
                    gra_char_vectors[m].x = (gra_char_vectors[m].x * 90) /100;
                    m++;
                }

            }
        }
    }

    // locate first vector of char
    nVect=charmap[ch]; 
    if (nVect == 0) return; // char not found, nothing to draw

    // here ww and hh = character width and heght
    // area height = char height * 1.66; area width = char width * 1.5
    switch (chsize & 3) {
        case 3:  ww=64; hh= 72; break; 
        case 2:  ww=32; hh= 48; break; 
        case 1:  ww=16; hh= 24; break; 
        default: ww=8;  hh= 12; break; 
    }
    rotated=(chsize & 4); 
    // generate vectors for this char
    x0=x_base; y0=y_base; 
    while (1) {
        x=gra_char_vectors[nVect].x; if (x<0) break; // end of char vectors
        y=gra_char_vectors[nVect].y; 
        pen=gra_char_vectors[nVect].pen; 
        nVect++;
        if (rotated) {
            x1=x_base - hh * y / 500;  y1=y_base + ww * x / 500;  // adjust to wanted char size
        } else {
            x1=x_base + ww * x / 500;  y1=y_base + hh * y / 500;  // adjust to wanted char size
        }
        if (pen) {
            draw_vector_in_scope(  x0, y0, x1, y1, decay, intensity);
            *count_nVectors = (*count_nVectors) + 1; 
        }
        x0=x1; y0=y1; 
    }

}


// time in usec to draw a vector
int drawing_time(int x1, int x2, int y1, int y2)
{
    int dx, dy, d, usec; 

    dx=(x2-x1); if (dx < 0) dx=-dx; 
    dy=(y2-y1); if (dy < 0) dy=-dy; 
    d = (dx > dy) ? dx:dy; 
    usec = 48; 
    usec= 2 + usec * d / 512; 
    return usec; 
}


// parse cmd1 and cmd2 chars received from CPU to determine the command
static void parse_cmd(void)
{
    int n;

    gra_chrin.cmd=0;
    for(n=0; gra_cmd_table[n].scmd; n++) {
        // check if is command in string scmd
        if ((gra_chrin.cmd1 != gra_cmd_table[n].scmd[0]) || (gra_chrin.cmd2 != gra_cmd_table[n].scmd[1])) continue; 
        // yes, set the command number
        gra_chrin.cmd=gra_cmd_table[n].ncmd; 
        // check if needs params
        if (gra_cmd_table[n].has_params==2) {
            gra_chrin.state = 20; // state set to parse text string param
        } else if (gra_cmd_table[n].has_params==1) {
            gra_chrin.state = 10; // state set to parse numeric param
        } else {
            gra_chrin.state = -1; // state set to wait for terminator
        }
        break; 
    }
    if (gra_chrin.cmd==0) {
       sim_debug_gra ("unknown command %c%c' \n", gra_chrin.cmd1, gra_chrin.cmd2);
       gra_chrin.state = -1; // state set to wait for terminator
    }
}

void execute_cmd(void) 
{
    int n, filenum, found, x, y, ww, hh, rotated, count; 
    char c; 
    
   if (gra_chrin.cmd == CMD_EM) {  // Erase Memory
        gra_state.WritePointer=0; 
        gra_state.NameFileFlag=0; 
        gra_state.AuxFlag=0; 
        gra_state.xpen=0; 
        gra_state.ypen=1020; 
        memset(gra_state.mem, 0, sizeof(gra_state.mem)); 
        sim_debug_gra ("CMD: EM - Erase Memory (size %d) \n", MAX_MEM);
   } else if (gra_chrin.cmd == CMD_EN) {  // Erase Names
        for (n=0; n<MAX_MEM; n++) gra_state.mem[n].filenum=0; 
        sim_debug_gra ("CMD: EN - Erase Names from memory (size %d) \n", MAX_MEM);
   } else if (gra_chrin.cmd == CMD_EX) {  // Erase auXiliary
        for (n=0; n<MAX_MEM; n++) gra_state.mem[n].Aux=0; 
        sim_debug_gra ("CMD: EX - Erase Aux from memory (size %d) \n", MAX_MEM);
   } else if (gra_chrin.cmd == CMD_BM) {  // Blank Memory
        gra_state.blank=1; 
        sim_debug_gra ("CMD: BM - Blank memory \n");
   } else if (gra_chrin.cmd == CMD_UM) {  // Unblank Memory
        gra_state.blank=0; 
        sim_debug_gra ("CMD: UM - UnBlank memory \n");
   } else if (gra_chrin.cmd == CMD_NF) {  // Name File
        gra_state.NameFileFlag=1; 
        gra_state.NameFileNum=gra_chrin.p0 & (MAX_FILES-1); // param is the file num
        sim_debug_gra ("CMD: NF - Name File to %d \n", gra_state.NameFileNum);
   } else if (gra_chrin.cmd == CMD_SN) {  // Stop Naming
        gra_state.NameFileFlag=0; 
        sim_debug_gra ("CMD: SN - Stop Naming \n");
   } else if (gra_chrin.cmd == CMD_FL) {  // Find Location
        n=gra_chrin.p0 & (MAX_MEM-1); // param is the memory location
        gra_state.WritePointer=n; 
        sim_debug_gra ("CMD: FL - Find Location: Set loc write pointer to %d \n", gra_state.WritePointer);
   } else if (gra_chrin.cmd == CMD_BF) {  // Blank File
        filenum=gra_chrin.p0 & (MAX_FILES-1); // param is the file number 
        count=0; 
        for (n=0; n<MAX_MEM; n++) {
            if (gra_state.mem[n].filenum != filenum) continue; 
            count++;
            gra_state.mem[n].blanked=1; 
            if (gra_state.AuxFlag)      gra_state.mem[n].Aux=gra_state.Aux; // assign the current aux value
        }
        sim_debug_gra ("CMD: BF - Blank File: blanked %d locations \n", count);
   } else if (gra_chrin.cmd == CMD_UF) {  // Unblank File
        filenum=gra_chrin.p0 & (MAX_FILES-1); // param is the file number 
        count=0; 
        for (n=0; n<MAX_MEM; n++) {
            if (gra_state.mem[n].filenum != filenum) continue; 
            count++;
            gra_state.mem[n].blanked=0; 
            if (gra_state.AuxFlag)      gra_state.mem[n].Aux=gra_state.Aux; // assign the current aux value
        }
        sim_debug_gra ("CMD: UF - UnBlank File: unblanked %d locations \n", count);
   } else if (gra_chrin.cmd == CMD_EF) {  // Erase File
        filenum=gra_chrin.p0 & (MAX_FILES-1); // param is the file number 
        count=0; 
        for (n=0; n<MAX_MEM; n++) {
            if (gra_state.mem[n].filenum != filenum) continue; 
            count++;
            gra_state.mem[n].x0=0; 
            gra_state.mem[n].y0=0; 
            gra_state.mem[n].x=0; 
            gra_state.mem[n].y=0; 
            gra_state.mem[n].ch=0; 
            gra_state.mem[n].chsize=0; 
            gra_state.mem[n].pen=0; 
            gra_state.mem[n].blanked=0; 
            gra_state.mem[n].Aux=0; 
            gra_state.mem[n].usec=0; 
        }
        gra_state.WritePointer=0; 
        sim_debug_gra ("CMD: EF - Erase File: erased %d locations, Write pointer set to 0 \n", count);
   } else if (gra_chrin.cmd == CMD_FF) {  // Find File
        filenum=gra_chrin.p0 & (MAX_FILES-1); // param is the file number 
        found=0; 
        for (n=0; n<MAX_MEM; n++) {
            if (gra_state.mem[n].filenum != filenum) continue; 
            found=n; 
            break; 
        }
        gra_state.WritePointer=found; 
        sim_debug_gra ("CMD: FF - Find File: Write pointer set to location %d \n", gra_state.WritePointer);
   } else if (gra_chrin.cmd == CMD_WX) {  // Write auXiliary
        gra_state.AuxFlag=1; 
        gra_state.Aux=gra_chrin.p0 & 15; // param is the aux value
        sim_debug_gra ("CMD: WX - Write Aux: set %d \n", gra_state.Aux);
   } else if (gra_chrin.cmd == CMD_SX) {  // Stop auXiliary
        gra_state.AuxFlag=0; 
        sim_debug_gra ("CMD: SN - Stop Aux \n");
   } else if (gra_chrin.cmd == CMD_PE) {  // Pen Enable
        n=gra_chrin.p0 & 1; 
        gra_state.pen=n; 
        sim_debug_gra ("CMD: PE - Pen Enable: set to %d \n", gra_state.pen);
   } else if (gra_chrin.cmd == CMD_CS) {  // Character Size
        n=gra_chrin.p0 & 7; 
        gra_state.chsize=n; 
        sim_debug_gra ("CMD: CS - Char Size: set to %d \n", gra_state.chsize);
   } else if (gra_chrin.cmd == CMD_PA) {  // Plot Absolute
        x=gra_chrin.p0; y=gra_chrin.p; 
        if ((x>1020) ||(y>1020)) {
            sim_debug_gra ("CMD: PA - Plot Absolute coord out of range: x=%d, y=%d \n", x,y);
            return; 
        }
        n=gra_state.WritePointer++;
        gra_state.WritePointer &= (MAX_MEM-1); 
        if (gra_state.NameFileFlag) gra_state.mem[n].filenum=gra_state.NameFileNum; // assign the current file number
        if (gra_state.AuxFlag)      gra_state.mem[n].Aux=gra_state.Aux; // assign the current aux value
        gra_state.mem[n].x0=gra_state.xpen; 
        gra_state.mem[n].y0=gra_state.ypen; 
        gra_state.mem[n].x=gra_state.xpen=x; 
        gra_state.mem[n].y=gra_state.ypen=y; 
        gra_state.mem[n].ch=0; 
        gra_state.mem[n].chsize=0; 
        gra_state.mem[n].pen=gra_state.pen; 
        gra_state.mem[n].usec=drawing_time(gra_state.mem[n].x0, gra_state.mem[n].y0, 
                                           gra_state.mem[n].x,  gra_state.mem[n].y);   
        sim_debug_gra ("CMD: PA - Plot Absolute x=%d, y=%d, pen=%d, "
                                  "Write pointer set to location %d, blanked %d \n", 
            x,y,gra_state.pen,gra_state.WritePointer, gra_state.mem[n].blanked);
   } else if (gra_chrin.cmd == CMD_TX) {  // TeXt        
        c=gra_chrin.p0; 
        x=gra_state.xpen; 
        y=gra_state.ypen; 
        n=gra_state.WritePointer++;
        gra_state.WritePointer &= (MAX_MEM-1); 
        sim_debug_gra ("CMD: TX - Plot Char x=%d, y=%d, ch=%d '%c', CharSize=%d, "
                                  "Write pointer set to location %d, blanked %d \n", 
            x,y, c, c<32?32:c, gra_state.chsize, gra_state.WritePointer, gra_state.mem[n].blanked);
        if (gra_state.NameFileFlag) gra_state.mem[n].filenum=gra_state.NameFileNum; // assign the current file number
        if (gra_state.AuxFlag)      gra_state.mem[n].Aux=gra_state.Aux; // assign the current aux value
        gra_state.mem[n].x=gra_state.mem[n].x0=x; 
        gra_state.mem[n].y=gra_state.mem[n].y0=y; 
        gra_state.mem[n].ch=c; 
        gra_state.mem[n].chsize=(0x100 | gra_state.chsize); 
        gra_state.mem[n].pen=gra_state.pen=1; 
        gra_state.mem[n].usec=15 * (1+(gra_state.chsize & 3)); 
        // here ww and hh = character AREA
        // area height = char height * 1.66; area width = char width * 1.5
        switch (gra_state.chsize & 3) {
            case 3:  ww=96; hh=120; break; 
            case 2:  ww=48; hh= 80; break; 
            case 1:  ww=24; hh= 40; break; 
            default: ww=12; hh= 20; break; 
        }
        rotated=(gra_state.chsize & 4); 
        // advance xpen/ypen beam pos for next char
        if (c==8) {
            // backspace
            if (rotated) {gra_state.ypen -= ww; } // rotated chars
            else {gra_state.xpen -= ww; }
        } else if (c==9) {
            // cursor up
            if (rotated) {gra_state.xpen -= hh; } 
            else {gra_state.ypen += hh; }
        } else if (c==10) {
            // line feed -> cursor down
            if (rotated) {gra_state.xpen += hh; } 
            else {gra_state.ypen -= hh; }
        } else if (c==13) {
            // carriage return 
            if (rotated) {gra_state.ypen = 0; } 
            else {gra_state.xpen = 0; }
        } else if ((c==11) || (c==12) || (c==14) || (c==15) || (c==16) || (c==30)) {
            // text special plotting characters. Char is draw arroung the currentbeam pot
            // Beam is left at center of char, it is not moved
            x=ww/3; y=hh * 3/10; // center of char
            if (rotated) {
                gra_state.mem[n].x-=y;
                gra_state.mem[n].y-=x;
            } else {
                gra_state.mem[n].x-=x;
                gra_state.mem[n].y-=y;
            }
        } else {
            // regular char
            if (rotated) {
                gra_state.ypen += ww; 
            }else {
                gra_state.xpen += ww; 
            }
        }
        if (gra_state.xpen < 0) gra_state.xpen=0; 
        if (gra_state.xpen > 1020) gra_state.xpen=1020; 
        if (gra_state.ypen < 0) gra_state.ypen=0; 
        if (gra_state.ypen > 1020) gra_state.ypen=1020; 
        sim_debug_gra ("CMD: TX - Beam Pos set to x=%d, y=%d, \n", 
            gra_state.xpen, gra_state.ypen);   
   }
   gra_chrin.p0=gra_chrin.p=0; // clear params for nect command
}

// get a char received from CPU, at it to command string, parse, and execute it
void HP1350_process_char_from_cpu(int chr_from_CPU)
{
    char c=chr_from_CPU; 

    sim_debug_gra ("recv chr %d '%c' \n", c, c<32?32:c);
    if (c==0) return; // ignore null chars
    if (c==20) return; // ignore DC4 char

    if (gra_chrin.state == 20) { // text mode: expecting text string's char
        if (c == 3) { 
            // c is ETX (control-C) -> signals end of text
            // c can also be an alternate char set by SET SCOPE ETX, e.g. "SET SCOPE ETX=|" 
            // this to allows using simple basic that has no CHR$ char
            sim_debug_gra ("ETX end of text string \n");
            gra_chrin.state=-1; // state set to wait for terminator
        } else {
            sim_debug_gra ("add char to vector list \n");
            // add char 
            gra_chrin.p0=c; // save char to draw as param
            execute_cmd(); 
        }
        return; 
    } else if ((c==';') && (gra_chrin.cmd == CMD_PA)) { // recv second param (y-coord)
        sim_debug_gra ("end of second param p=%d \n", gra_chrin.p);
        execute_cmd(); // add vector to list
        return; 
    } else if ((c==':') || (c==13) || (c==10)) { // normal mode: check for terminator
        // terminator 
        if ((gra_chrin.cmd == CMD_PA) || (gra_chrin.cmd == CMD_TX)) gra_chrin.cmd=0; // TX and PA commands already exec
        if (gra_chrin.cmd) execute_cmd(); // execute command if any
        gra_chrin.state=0; // sey state to start a new command read
        gra_chrin.cmd=0;   // set as no command to be processed
        return; 
    }

    if (gra_chrin.state == 0) { // expecting first char of a command
        c = sim_toupper(c);
        if ((c<'A') || (c>'Z')) c = '?'; 
        gra_chrin.cmd1=c; 
        gra_chrin.state++;
    } else if (gra_chrin.state == 1) { // expecting second char of a command
        c = sim_toupper(c); 
        if ((c<'A') || (c>'Z')) c = '?'; 
        gra_chrin.cmd2=c; 
        sim_debug_gra ("cmd recv %c%c \n", gra_chrin.cmd1, gra_chrin.cmd2);
        parse_cmd(); // determine the cmd received, and set next char in state according to it
        gra_chrin.p=0;
        gra_chrin.p0=0; 
    } else if (gra_chrin.state == 10) { // expecting param 
        if (c == 32) return; // ignore spaces
        if (c == ',') { // end of param
            gra_chrin.p0=gra_chrin.p;
            sim_debug_gra ("end of param p0=%d \n", gra_chrin.p0);
            gra_chrin.p=0;
        } else if ((c>='0') && (c<='9')) {
            gra_chrin.p = gra_chrin.p * 10 + c - '0'; 
            if (gra_chrin.p > 9999) gra_chrin.p -= 10000; // keep last 4 digits only
        } else {
            sim_debug_gra ("invalid param digit, discard char \n");
        }
    } else if (gra_chrin.state == -1) { // expecting terminator
        sim_debug_gra ("expecting terminator, discard char \n");
    } else {
        sim_debug_gra ("invalid chr_in state, discard char \n");
    }
}



