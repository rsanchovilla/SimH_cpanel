/* ibm360_con.c: IBM 360 Inquiry console.

   Copyright (c) 2017-2020, Richard Cornwell

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
   RICHARD CORNWELL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   This is the standard inquiry or console interface.

   These units each buffer one record in local memory and signal
   ready when the buffer is full or empty. The channel must be
   ready to recieve/transmit data when they are activated since
   they will transfer their block during chan_cmd. All data is
   transmitted as BCD characters.

*/

#include "ibm360_defs.h"
#include "sim_defs.h"
#include <ctype.h>

#ifdef NUM_DEVS_CON

/* Held in u3 */
#define CHN_SNS         0x04    /* Sense command */
#define CON_WR          0x01    /* Write console */
#define CON_ACR         0x09    /* Auto carrage return */
#define CON_RD          0x0a    /* Read console */
#define CON_NOP         0x03    /* No op command */
#define CON_MSK         0x0f    /* Command mask */

/* Status held in u3 */
#define CON_INPUT       0x100   /* Input ready for unit */
#define CON_CR          0x200   /* Output at beginning of line */
#define CON_REQ         0x400   /* Request key pressed */
#define CON_OUTPUT      0x800   /* Output characters since R */
#define CON_CANCEL      0x1000  /* Control C pressed */

/* Upper 11 bits of u3 hold the device address */

/* Input buffer pointer held in u4 */

/* in u5 packs sense byte 0,1 and 3 */
/* Sense byte 0 */
#define SNS_CMDREJ      0x80    /* Command reject */
#define SNS_INTVENT     0x40    /* Unit intervention required */

#define CMD     u3
#define IPTR    u4
#define SNS     u5

/* std devices. data structures

   con_dev       Console device descriptor
   con_unit       Console unit descriptor
   con_reg       Console register list
   con_mod       Console modifiers list
*/

struct _con_data
{
    uint8              ibuff[145];       /* Input line buffer */
    uint8              inptr;
}
con_data[NUM_DEVS_CON];

struct _con_data_out
{
    char               slin[145+1];       /* line buffer of chars that CPU send to console */
    uint8              nlen;
}
con_data_out[NUM_DEVS_CON];


uint8  con_startcmd(UNIT *, uint8);
uint8  con_haltio(UNIT *);
void                con_ini(UNIT *, t_bool);
t_stat              con_srv(UNIT *);
t_stat              con_attach(UNIT *, CONST char *);
t_stat              con_detach(UNIT *);
t_stat              con_help(FILE *, DEVICE *, UNIT *, int32, const char *);
const char         *con_description(DEVICE *d);

UNIT                con_unit[] = {
    {UDATA(con_srv, UNIT_ATT, 0), 0, UNIT_ADDR(0x1F)},       /* A */
};

MTAB                con_mod[] = {
    {MTAB_XTD | MTAB_VUN | MTAB_VALR, 0, "DEV", "DEV", &set_dev_addr,
        &show_dev_addr, NULL},
    {0}
};

struct dib con_dib = { 0xFF, 1, NULL, con_startcmd, con_haltio, con_unit, con_ini};

DEVICE              con_dev = {
    "INQ", con_unit, NULL, con_mod,
    NUM_DEVS_CON, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, NULL, &con_attach, &con_detach,
    &con_dib, DEV_UADDR | DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &con_help, NULL, NULL, &con_description
};

#if defined(CPANEL)
// vars that notify the state of console printing process
// buffer to hold last printed lines on con
char   ConsPrintOut[CONS_LPT_COLUMNS * ConsPrintOutMAX];
int    ConsPrintOutCount = 0; // total number of lines printed
int    ConsCharsOutCount = 0; // total number of characters printed in line (range 0..79)
int    ConsSomethingPrinted = 0; // set to 1 signal refresh should redraw control on screen
int    bProceed = 0; // status of proceed light
int    bRequestPending = 0; // request button pressed, but pending to be serviced 
char   ConsKeybBuffer[128]; // keyb buffer of chars sent from cpanel console
int    ConsGolfBallAction = 0; // action Golf ball printing head should animate
int    ConsGolfBallPosition = 0; // column Golf ball is doing action
uint32 tm0ConsGolfBallAction = 0; // when action was started
// vars used to simulate the timimg of device to operate a real speed respect wallclock
// applies only when cpanel is visible (cpanel_on), and on visible on gui 
uint32 ConsPrint_tm0 = 0;          // set to sim_os_msec, checked by con_svr to simulate wallclock time needed by device to operate
uint32 ConsPrint_msec = 0;         // duration of device operation
uint32 ConsPollKeyb_tm0 = 0;       // timestamp when last call to sim_poll_kbd() was done
uint32 ConsPollKeyb_msec = 0;      // time remaining for next call to sim_poll_kbd() 

// same as sim_putchar, to print a char in control panel 
void cpanel_putchar(char c)
{ 
    int n;
    // last ConsPrintOutMAX printed lines will be saved on circular buffer ConsPrintOut
    // acts as mapped memory for 20 last lines printed
    // ConsPrintOutCount points to first free line
    // ConsCharsOutCount points to first free char in line
    // ConsSomethingPrinted indicates printer activity so refresh must redraw the printed paper (even if number of lines/number of chars has not changed)
    if (ConsPrintOutCount == 0) ConsPrintOutCount=1; // points to first free line
    if (c==13) {
        // <CR> char 13 is end of line. Scroll buffer to open a new line
        // clear free line pointed by ConsPrintOutCount
        n = CONS_LPT_COLUMNS * (ConsPrintOutCount % ConsPrintOutMAX); 
        memset(&ConsPrintOut[n], 32, CONS_LPT_COLUMNS); // clear line
        // set vars for animation
        ConsGolfBallAction   = 1;   // 1= <CR> is the action Golf ball printing head should animate 
        ConsGolfBallPosition = ConsCharsOutCount; // column 0..79 Golf ball is doing action
        tm0ConsGolfBallAction=sim_os_msec(); // when animation starts
        // incr num of lines printed and signal should refresh
        ConsCharsOutCount=0;      // next char to be printed on first colum of new line
        ConsPrintOutCount++;      
        ConsSomethingPrinted=1;   // signal refresh should refresh the paper on cpanel
    } else if ((c>=32) && (c<=127)) {
        // add printable char to line
        n = CONS_LPT_COLUMNS * ( (ConsPrintOutCount-1) % ConsPrintOutMAX); // set the line
        n = n + ConsCharsOutCount % CONS_LPT_COLUMNS; // set the line (modulo with max num of cols as a safety
        if ((c >= 'a') && (c <= 'z')) c = c - 'a' + 'A';
        ConsPrintOut[n]=c; 
        // set vars for animation
        if (c > 32) {
            ConsGolfBallAction   = 2;   // 2= printChar is the action Golf ball printing head should animate 
            ConsGolfBallPosition = ConsCharsOutCount;    // column Golf ball is doing action
            tm0ConsGolfBallAction=sim_os_msec(); // when animation starts
        } else {
            // printing space is just advancing the head to the new pos
            ConsGolfBallAction   = 3;   // 30 = Set Golf ball printing head stoped at colum ConsCharsOutCount
        }
        // increment count of chars printed in line
        ConsCharsOutCount++;
        if (ConsCharsOutCount >= CONS_LPT_COLUMNS) {
            // if reached end of line in console printer, add new line
            cpanel_putchar(13);
        }
        ConsSomethingPrinted=1; // something printed
    } else if ((c==0177) || (c=='\b')) {
        // backspace
        if (ConsCharsOutCount) {
            ConsCharsOutCount--;    // back one char
            // set vars for animation
            ConsGolfBallAction   = 3;   // 30 = Set Golf ball printing head stoped at colum ConsCharsOutCount
        }
    } else c=0; // no printable char
    // set time needed to print the char in console
    if (c!=0) {
       // The IBM 1052 [...]  speed up tp 14.8 characters per secons
       // -> print one char each 67.5 msec. Carriage returns takes between 7 and 3 char times
       ConsPrint_tm0  = sim_os_msec();          // set to sim_os_msec, checked by con_svr to simulate wallclock time needed by cdr to operate
       if (c == 13) {
           // duration of cons operation (CR)
           n=ConsGolfBallPosition;
           if ((n<1) || (n > CONS_LPT_COLUMNS)) n=CONS_LPT_COLUMNS; 
           ConsPrint_msec = 68 * 3 + 68 * (7-3) * n / CONS_LPT_COLUMNS;   
       } else {
           // duration of cons operation (print one char)
           ConsPrint_msec = 68;                 
       }
    } 
}

extern uint32       idle_stop_tm0;          // sim_os_msec when start counting for idle time
                                            // used on CP_PUTCHAR to reset idle countdown because a char is printed on console

#define CP_PUTCHAR(c)     idle_stop_tm0=0; if ((cpanel_on) && (uptr - con_unit == 0)) cpanel_putchar(c);  // add char to console control panel's array of chars to print
#else
#define CP_PUTCHAR(c)    {}  // do nothing
#endif


/*
 * Console printer routines.
 */
void
con_ini(UNIT *uptr, t_bool f) {
     int                 u = (uptr - con_unit);
     con_data[u].inptr = 0;
     con_data_out[u].nlen = 0;
     uptr->CMD &= ~(CON_MSK|CON_REQ|CON_INPUT|CON_CR|CON_CANCEL);
     uptr->SNS = 0;
     sim_activate(uptr, 1000);
}

uint8  con_startcmd(UNIT *uptr, uint8 cmd) {
    int                 u = (uptr - con_unit);

    sim_debug(DEBUG_CMD, &con_dev, "Console %d: Cmd %x %x\n", u, cmd, uptr->CMD);
    if ((uptr->CMD & CON_MSK) != 0)
        return SNS_BSY;

    if ((cmd & 0xf0) != 0) {
        uptr->SNS |= SNS_CMDREJ;
        return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK;
    }

    switch (cmd & 0x7) {
    case 2:                        /* Read command */
         sim_debug(DEBUG_CMD, &con_dev, "Console %d: Cmd RD\n", u);
         if (uptr->CMD & CON_REQ) {
              uptr->CMD &= ~CON_REQ;
              return SNS_ATTN;
         }

         if ((uptr->CMD & CON_INPUT) == 0 &&
                (con_data[u].inptr == 0 || uptr->CMD & CON_CR)) {
             /* Activate input so we can get response */
             if ((uptr->CMD & CON_OUTPUT) != 0) {
                sim_putchar('\r'); 
                sim_putchar('\n'); CP_PUTCHAR(13); 
                uptr->CMD &= ~CON_OUTPUT;
             }
             sim_putchar('I');
             sim_putchar(' ');
         }
#if defined(CPANEL)
         if (uptr - con_unit == 0) { 
             bProceed=1; bRequestPending=0; 
             memset(ConsKeybBuffer, 0, sizeof(ConsKeybBuffer)); // clear keyb buffer when Proceed goes on
         }
#endif

         uptr->IPTR = 0;
         uptr->CMD |= cmd & CON_MSK;
         uptr->SNS = 0;
         return 0;

    case 1:                    /* Write command */
         sim_debug(DEBUG_CMD, &con_dev, "Console %d: Cmd WR\n", u);
         if (uptr->CMD & CON_REQ) {
              uptr->CMD &= ~CON_REQ;
              return SNS_ATTN;
         }
         uptr->CMD |= cmd & CON_MSK;
         uptr->SNS = 0;
         if (uptr->CMD & CON_CR) {
            sim_putchar('R');
            sim_putchar(' ');
            uptr->CMD &= ~CON_CR;
            uptr->CMD |= CON_OUTPUT;
         }
         return 0;

    case 3:              /* Control */
         sim_debug(DEBUG_CMD, &con_dev, "Console %d: Cmd NOP\n", u);
         if (uptr->CMD & CON_REQ) {
              uptr->CMD &= ~CON_REQ;
              return SNS_ATTN;
         }
         uptr->SNS = 0;
         return SNS_CHNEND|SNS_DEVEND;

    case 0:               /* Status */
         return 0;

    case 4:              /* Sense */
         uptr->CMD |= cmd & CON_MSK;
         return 0;

    default:              /* invalid command */
         sim_debug(DEBUG_CMD, &con_dev, "Console %d: ERROR: Invalid CMD %x\n", u, cmd);
         uptr->SNS |= SNS_CMDREJ;
         break;
    }

    if (uptr->SNS)
        return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK;
    return SNS_CHNEND|SNS_DEVEND;
}

/*
 * Handle halt I/O instruction by stoping running command.
 */
uint8  con_haltio(UNIT *uptr) {
    uint16         addr = GET_UADDR(uptr->CMD);
    DEVICE         *dptr = find_dev_from_unit(uptr);
    int            u = (uptr - con_unit);
    int            cmd = uptr->CMD & 0xff;

    sim_debug(DEBUG_CMD, dptr, "Console: HLTIO inq %x\n", cmd);

    switch (cmd) {
    case 0:
    case 0x4:
         /* Short commands nothing to do */
         break;

    case CON_WR:
    case CON_ACR:
    case CON_RD:
         uptr->CMD &= ~(CON_MSK|CON_INPUT|CON_CANCEL);
         con_data[u].inptr = 0;
         chan_end(addr, SNS_CHNEND|SNS_DEVEND);
#if defined(CPANEL)
         if (uptr - con_unit == 0) bProceed=0; 
#endif
         break;
    }
    return 1;
}


/* Handle transfer of data for printer */
t_stat
con_srv(UNIT *uptr) {
    uint16              addr = GET_UADDR(uptr->CMD);
    int                 u = (uptr - con_unit);
    int                 cmd = uptr->CMD & CON_MSK;
    t_stat              r = SCPE_ARG;       /* Force error if not set */
    uint8               ch;
    int                 i;
    int                 delay = 1000;
    char                console_lin[145+1];  

#if defined(CPANEL)
    SEND *snd;
    uint32 msec;
    
    // check if there is a scp SEND command with chars pending to be sent to console
    // (they are polled into sim_poll_kbd()). If so -> do not wait
    snd = sim_cons_get_send ();
    if ((snd) && (snd->extoff < snd->insoff)) {
        ConsPrint_tm0=0;    // send ongoing -> clear tm0 to process any pending command
        ConsPollKeyb_tm0=0; // send ongoing -> clear tm0 to poll keyboard 
    }

    if ((cpanel_on) && (bFastMode==0) && (uptr-con_unit==0) && (ConsPrint_tm0)) {
        // if cpanel GUI visible, and fastmode not selected, and device operation has a start timestamp ...
        uint32 msec = sim_os_msec() - ConsPrint_tm0;
        if (msec < ConsPrint_msec) {
            // operation still in progress ... check again later
            goto KeybPoll;
        }
        // operation terminated. Clear the timestamp
        ConsPrint_tm0 = 0; 
    }
#endif

    switch (cmd) {
    case 4:              /* Sense */
         sim_debug(DEBUG_CMD, &con_dev, "Console %d: Cmd SNS %02x\n", u, uptr->SNS);
         /* Check if request pending */
         ch = uptr->SNS;
         chan_write_byte(addr, &ch);
         chan_end(addr, SNS_CHNEND|SNS_DEVEND);
         uptr->CMD &= ~(CON_MSK);
         uptr->SNS = 0;
         break;

    case CON_WR:
    case CON_ACR:
       if (chan_read_byte(addr, &ch)) {
           if (cmd == CON_ACR) {
               sim_putchar('\r');
               sim_putchar('\n'); CP_PUTCHAR(13); 
               uptr->CMD |= CON_CR;
               uptr->CMD &= ~CON_OUTPUT;
           }
           uptr->CMD &= ~CON_MSK;
           chan_end(addr, SNS_CHNEND|SNS_DEVEND);
       } else {
           if (ch == 0x15) {
               sim_putchar('\r');
               sim_putchar('\n'); CP_PUTCHAR(13); 
               uptr->CMD |= CON_CR;
               uptr->CMD &= ~CON_OUTPUT;
           } else {
               ch = ebcdic_to_ascii[ch];
               if (ch != 0) {
                   if (!isprint(ch))
                       ch = '_';
                   sim_putchar(ch); CP_PUTCHAR(ch); 
                   uptr->CMD &= ~CON_OUTPUT;
                   if (con_data_out[u].nlen < 145) {
                       con_data_out[u].slin[con_data_out[u].nlen++]=ch;
                   }
               }
           }
       }
       if (uptr->CMD & CON_CR) {
           con_data_out[u].slin[con_data_out[u].nlen]=0;
           sim_debug(DEBUG_DETAIL, &con_dev, "Console %d line printed by cpu: %s\n", u, con_data_out[u].slin);
           con_data_out[u].nlen=0;
       }
       break;

    case CON_RD:
       if (uptr->CMD & CON_INPUT) {
           uptr->CMD &= ~CON_REQ;
           /* Check for empty line, or end of data */
           if (con_data[u].inptr == 0 || uptr->IPTR == con_data[u].inptr) {
                   uptr->CMD &= ~CON_INPUT;
                   con_data[u].inptr = 0;
                   cmd = 0;
                   uptr->CMD &= ~(CON_MSK);
                   sim_debug(DEBUG_CMD, &con_dev, "Console %d: devend\n", u);
                   if (uptr->CMD & CON_CANCEL) {
                       uptr->CMD &= ~CON_CANCEL;
                       chan_end(addr, SNS_CHNEND|SNS_DEVEND|SNS_UNITEXP);
                   } else {
                       chan_end(addr, SNS_CHNEND|SNS_DEVEND);
                   }
#if defined(CPANEL)
                   if (uptr - con_unit == 0) bProceed=0; 
#endif
                   break;
           }

           /* Grab next character and send it to CPU */
           ch = con_data[u].ibuff[uptr->IPTR++];
           sim_debug(DEBUG_CMD, &con_dev, "Console %d: rd %02x\n", u, ch);
           if (chan_write_byte(addr, &ch)) {
               uptr->CMD &= ~CON_INPUT;
               con_data[u].inptr = 0;
               cmd = 0;
               uptr->CMD &= ~(CON_MSK);
               sim_debug(DEBUG_CMD, &con_dev, "Console %d: devend input\n", u);
               if (uptr->CMD & CON_CANCEL) {
                   uptr->CMD &= ~CON_CANCEL;
                   chan_end(addr, SNS_CHNEND|SNS_DEVEND|SNS_UNITEXP);
               } else {
                   chan_end(addr, SNS_CHNEND|SNS_DEVEND);
               }
#if defined(CPANEL)
               if (uptr - con_unit == 0) bProceed=0; 
#endif
            }
         }
         break;
    }
#if defined(CPANEL)
KeybPoll:
    msec = sim_os_msec() - ConsPollKeyb_tm0;
    if ((ConsPollKeyb_tm0) && (msec < ConsPollKeyb_msec) && (bFastMode==0)) {
        // cmd.exe console keyboard operation already polled
        // do not poll again until ConsPollKeyb_msec has passed to avoid polling it too often
        r = 0;
    } else {
        // poll simh console cmd.exe
        r = sim_poll_kbd();
        // polled done. Set the timestamp
        ConsPollKeyb_tm0 = sim_os_msec();
        ConsPollKeyb_msec = 50; // poll host keyboard each 50 msec (20 times per second)
    }
    if (r & SCPE_KFLAG) {
        // should process char polled from host keyboard on cmd.exe console
    } else if (uptr - con_unit == 0) {
        // nothing typed in host keyboard on cmd.exe console, check cpanel console
        if (ConsKeybBuffer[0]) {
            int i; 
            r = ConsKeybBuffer[0] | SCPE_KFLAG; // char typed in host keyboard on cpanel console
            for (i=0; ConsKeybBuffer[i]; i++) ConsKeybBuffer[i]=ConsKeybBuffer[i+1]; // remove char from buf
        } else r=0; // nothing typed in host keyboard on cpanel console
    }
#else
    r = sim_poll_kbd();
#endif

    if (r & SCPE_KFLAG) {
       ch = r & 0377;
       if (ch == 030) { /* ^X Post external interrupt */  //YYY
           sim_debug(DEBUG_CMD, &con_dev, "Console %d: ^X Key (external interrupt)\n", u);
           post_extirq();
           goto EndOfKbdPoll;
       }
       if ((uptr->CMD & CON_INPUT) == 0) {
          /* Handle end of buffer */
          switch (ch) {
          case '\r':
          case '\n':
                sim_debug(DEBUG_DATA, &con_dev, "Console %d: Enter Key\n", u);
                uptr->CMD |= CON_INPUT;
                uptr->CMD |= CON_CR;
                uptr->CMD &= ~CON_OUTPUT;
                sim_putchar('\r');
                sim_putchar('\n'); CP_PUTCHAR(13); 
                
                // compose line user has typed on console and will be sent to cpu
                for (i=0;i<(int)(con_data[u].inptr);i++) {
                    if (i==145) break;
                    console_lin[i] = ebcdic_to_ascii[con_data[u].ibuff[i]];
                }
                console_lin[i] = 0;
                sim_debug(DEBUG_DETAIL, &con_dev, "Console %d typed by user: %s\n", u, console_lin);
               /* Fall through */

          case 033: /* request key */
                if (cmd != CON_RD) {
                    sim_debug(DEBUG_CMD, &con_dev, "Console %d: Esc Key (request)\n", u);
                    uptr->CMD |= CON_REQ;
                }
                break;
          case 0177:
          case '\b':
                if (con_data[u].inptr != 0) {
                     con_data[u].inptr--;
                     sim_putchar('\b');
                     sim_putchar(' ');
                     sim_putchar('\b'); CP_PUTCHAR('\b'); 
                }
                break;
           case 03:  /* ^C */
                uptr->CMD |= CON_CANCEL|CON_INPUT;
                CP_PUTCHAR(13); 
                break;

           case 025: /* ^U clear line */
                for (i = con_data[u].inptr; i> 0; i--) {
                    sim_putchar('\b');
                    sim_putchar(' ');
                    sim_putchar('\b');
                }
                CP_PUTCHAR(13); // on cpanel console, just end a <CR> to open a new fresh line
                con_data[u].inptr = 0;
                break;

          default:
                sim_debug(DEBUG_DATA, &con_dev, "Console %d: key '%c'\n", u, ch);
                if (con_data[u].inptr < sizeof(con_data[u].ibuff)) {
                    int ch2 = ascii_to_ebcdic[ch];
                    if (ch2 == 0xff) {
                       sim_putchar('\007'); // char not defined in ebcdic table
                       break;
                    }
                    sim_putchar(ch);  CP_PUTCHAR(ch); 
                    con_data[u].ibuff[con_data[u].inptr++] = ch2;
                }
                break;
          }
       } else {
           if (cmd == CON_RD && ch == 03) { /* Cancel */
               chan_end(addr, SNS_CHNEND|SNS_DEVEND|SNS_UNITEXP);
               uptr->CMD &= ~(CON_INPUT|CON_CANCEL);
               con_data[u].inptr = 0;
               cmd = 0;
#if defined(CPANEL)
               if (uptr - con_unit == 0) bProceed=0; 
               CP_PUTCHAR(13);
#endif
           } else {
                sim_debug(DEBUG_CMD, &con_dev, "Console %d: error %x\n", u, cmd);
                if (cmd == 0)
                    uptr->CMD |= CON_REQ;               
                sim_putchar('\007'); // BEEP
           }
        }
    }
    if (cmd == 0 && uptr->CMD & CON_REQ) {
          sim_debug(DEBUG_CMD, &con_dev, "Console %d: setattn %x\n", u, addr);
          set_devattn(addr, SNS_ATTN);
#if defined(CPANEL)
          if (uptr - con_unit == 0) bRequestPending=1; 
#endif         
          uptr->CMD &= ~CON_REQ;
    }
EndOfKbdPoll:
    sim_activate(uptr, delay);
    return SCPE_OK;
}

t_stat
con_attach(UNIT *uptr, CONST char *file)
{
    return SCPE_OK;
}

t_stat
con_detach(UNIT *uptr)
{
    return SCPE_OK;
}

t_stat
con_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "1050 Console Terminal\n");
   fprintf (st, "This is the interface from the operator to the system. To request the\n");
   fprintf (st, "system to accept input press the <esc> key and wait until the system\n");
   fprintf (st, "responds with a line with I as the first character. When you have\n");
   fprintf (st, "finished typing your line, press return or enter key. Backspace will\n");
   fprintf (st, "delete the last character. All responses from the system are prefixed\n");
   fprintf (st, "with a R and blank as the first character. Not all operating systems\n");
   fprintf (st, "require the use of <esc> to enter data\n");
   fprintf (st, "Pressing control-X will issue a external interrupt to the CPU\n");
   return SCPE_OK;
}

const char * con_description(DEVICE *dptr)
{
   return "1050 Console Terminal";
}


#endif


