/* ibm360_mt.c: IBM 360 2400 Magnetic tape controller

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

   Magnetic tapes are represented as a series of variable records
   of the form:

        32b byte count
        byte 0
        byte 1
        :
        byte n-2
        byte n-1
        32b byte count

   If the byte count is odd, the record is padded with an extra byte
   of junk.  File marks are represented by a byte count of 0.
*/

#include "ibm360_defs.h"
#include "sim_tape.h"

#ifdef NUM_DEVS_MT
#define BUFFSIZE       (64 * 1024)
#define MTUF_9TR       (1 << MTUF_V_UF)
#define DEV_BUF_NUM(x)  (((x) & 07) << DEV_V_UF)
#define GET_DEV_BUF(x)  (((x) >> DEV_V_UF) & 07)
#define MT_BUSY         (1 << (MTUF_V_UF + 1))    /* Flag to send a CUE */
#define UNIT_MT(x)     UNIT_ATTABLE | UNIT_DISABLE | UNIT_ROABLE | MTUF_9TR | \
                          DEV_BUF_NUM(x)

#define MT_WRITE            0x01       /* Write command */
#define MT_READ             0x02       /* Read command */
#define MT_RDBK             0x0c       /* Read Backward */
#define MT_SENSE            0x04       /* Sense command */
#define MT_REW              0x07       /* Rewind command */
#define MT_RUN              0x0f       /* Rewind and unload */
#define MT_ERG              0x17       /* Erase Gap */
#define MT_WTM              0x1f       /* Write Tape Mark */
#define MT_BSR              0x27       /* Back space record */
#define MT_BSF              0x2f       /* Back space file */
#define MT_FSR              0x37       /* Forward space record */
#define MT_FSF              0x3f       /* Forward space file */
#define MT_MODE             0x03       /* Mode command */
#define MT_MODEMSK          0x07       /* Mode Mask */

#define MT_MDEN_200         0x00       /* 200 BPI mode 7 track only */
#define MT_MDEN_556         0x40       /* 556 BPI mode 7 track only */
#define MT_MDEN_800         0x80       /* 800 BPI mode 7 track only */
#define MT_MDEN_1600        0xc0       /* 1600 BPI mode 9 track only */
#define MT_MDEN_MSK         0xc0       /* Density mask */

#define MT_CTL_MSK          0x38       /* Mask for control flags */
#define MT_CTL_NOP          0x00       /* Nop control mode */
#define MT_CTL_NRZI         0x08       /* 9 track 800 bpi mode */
#define MT_CTL_RST          0x10       /* Set density, odd, convert on, trans off */
#define MT_CTL_NOP2         0x18       /* 9 track 1600 NRZI mode */
#define MT_CTL_MD0          0x20       /* Set density, even, convert off, trans off */
#define MT_CTL_MD1          0x28       /* Set density, even, convert off, trans on */
#define MT_CTL_MD2          0x30       /* Set density, odd, convert off, trans off */
#define MT_CTL_MD3          0x38       /* Set density, odd, convert off, trans on */

/* in u3 is device command code and status */
#define MT_CMDMSK           0x0003f       /* Command being run */
#define MT_READDONE         0x00400       /* Read finished, end channel */
#define MT_MARK             0x00800       /* Sensed tape mark in move command */
#define MT_ODD              0x01000       /* Odd parity */
#define MT_TRANS            0x02000       /* Translation turned on ignored 9 track  */
#define MT_CONV             0x04000       /* Data converter on ignored 9 track  */
#define MT_CMDREW           0x10000       /* Rewind being done */
#define MT_CMDRUN           0x20000       /* Unload being done */

/* Upper 11 bits of u3 hold the device address */

/* in u4 is current buffer position */

/* in u5 packs sense byte 0,1 and 3 */
/* Sense byte 0 */
#define SNS_CMDREJ       0x80       /* Command reject */
#define SNS_INTVENT      0x40       /* Unit intervention required */
#define SNS_BUSCHK       0x20       /* Parity error on bus */
#define SNS_EQUCHK       0x10       /* Equipment check */
#define SNS_DATCHK       0x08       /* Data Check */
#define SNS_OVRRUN       0x04       /* Data overrun */
#define SNS_WCZERO       0x02       /* Write with no data */
#define SNS_CVTCHK       0x01       /* Data conversion error */

/* Sense byte 1 */
#define SNS_NOISE        0x80       /* Noise record */
#define SNS_TUASTA       0x40       /* Selected and ready */
#define SNS_TUBSTA       0x20       /* Not ready, rewinding. */
#define SNS_7TRACK       0x10       /* Seven track unit */
#define SNS_LOAD         0x08       /* Load Point */
#define SNS_WR           0x04       /* Unit write */
#define SNS_WRP          0x02       /* No write ring */
#define SNS_DENS         0x01       /* Density error 9tr only */

/* Sense byte 2 */
#define SNS_BYTE2        0x03       /* Not supported feature */

/* Sense byte 3 */
#define SNS_VRC          0x80       /* Veritical parity error */
#define SNS_LRCR         0x40       /* Logituntial parity error */
#define SNS_SKEW         0x20       /* Skew */
#define SNS_CRC          0x10       /* CRC error. 9t only */
#define SNS_SKEWVRC      0x08       /* VRC Skew */
#define SNS_PE           0x04       /* Phase encoding */
#define SNS_BACK         0x01       /* tape in backward status */

#define SNS_BYTE4        0x00       /* Hardware errors not supported */
#define SNS_BYTE5        0x00       /* Hardware errors not supported */

#define MT_CONV1         0x40
#define MT_CONV2         0x80
#define MT_CONV3         0xc0

/* u6 holds the packed characters and unpack counter */
#define BUF_EMPTY(u)  (u->hwmark == 0xFFFFFFFF)
#define CLR_BUF(u)     u->hwmark =  0xFFFFFFFF

#define CMD    u3
#define POS    u4
#define SNS    u5
#define CPOS   u6

uint8               mt_startio(UNIT *uptr);
uint8               mt_startcmd(UNIT *uptr, uint8 cmd);
t_stat              mt_srv(UNIT *);
t_stat              mt_boot(int32, DEVICE *);
void                mt_ini(UNIT *, t_bool);
t_stat              mt_boot(int32, DEVICE *);
t_stat              mt_attach(UNIT *, CONST char *);
t_stat              mt_detach(UNIT *);
#if defined(CPANEL)
t_stat              mt_rew(UNIT * uptr, int32 val, CONST char *cptr,void *desc);
t_stat              mt_set_len (UNIT *uptr, int32 val, CONST char *cptr, void *desc);
t_stat              mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc);
t_stat              mt_set_cabinet_door(UNIT * uptr, int32 val, CONST char *cptr,void *desc);
#endif
t_stat              mt_help (FILE *, DEVICE *, UNIT *, int32, const char *);
const char         *mt_description (DEVICE *);


/* One buffer per channel */
uint8               mt_buffer[NUM_DEVS_MT][BUFFSIZE];
uint8               mt_busy[NUM_DEVS_MT];

MTAB                mt_mod[] = {
    {MTUF_WLK, 0, "write enabled", "WRITEENABLED", NULL},
    {MTUF_WLK, MTUF_WLK, "write locked", "LOCKED", NULL},
    {MTUF_9TR, 0, "7 track", "7T", NULL},
    {MTUF_9TR, MTUF_9TR, "9 track", "9T", NULL},
    {MTAB_XTD | MTAB_VUN, 0, "FORMAT", "FORMAT",        &sim_tape_set_fmt, &sim_tape_show_fmt, NULL},
    {MTAB_XTD | MTAB_VUN | MTAB_VALR, 0, "DEV", "DEV",  &set_dev_addr, &show_dev_addr, NULL},
#if defined(CPANEL)
    {MTAB_XTD | MTAB_VUN , 0, NULL,     "REWIND",       &mt_rew, NULL, NULL, "Rewind tape"},  
    {MTAB_XTD | MTAB_VUN,  0, "LENGTH", "LENGTH",       &mt_set_len, &mt_show_len, NULL,
                                                                             "Set tape medium length (50 to 10000 feet)" },
    {MTAB_XTD | MTAB_VUN , 0, NULL,     "DOORCLOSED",   &mt_set_cabinet_door, NULL, NULL, "Close cabinet door on Control Panel GUI"},  
    {MTAB_XTD | MTAB_VUN , 1, NULL,     "DOOROPEN",     &mt_set_cabinet_door, NULL, NULL, "Open cabinet door on Control Panel GUI"},  
#endif
    {0}
};

UNIT                mta_unit[] = {
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x180)},       /* 0 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x181)},       /* 1 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x182)},       /* 2 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x183)},       /* 3 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x184)},       /* 4 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x185)},       /* 5 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x186)},       /* 6 */
    {UDATA(&mt_srv, UNIT_MT(0), 0), 0, UNIT_ADDR(0x187)},       /* 7 */
};

struct dib mta_dib = { 0xF8, NUM_UNITS_MT, mt_startio, mt_startcmd, NULL, mta_unit, mt_ini};

DEVICE              mta_dev = {
    "MTA", mta_unit, NULL, mt_mod,
    NUM_UNITS_MT, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, &mt_boot, &mt_attach, &mt_detach,
    &mta_dib, DEV_BUF_NUM(0) | DEV_DISABLE | DEV_DEBUG | DEV_TAPE, 0, dev_debug,
    NULL, NULL, &mt_help, NULL, NULL, &mt_description
};

#if NUM_DEVS_MT > 1
UNIT                mtb_unit[] = {
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x280)},       /* 0 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x281)},       /* 1 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x282)},       /* 2 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x283)},       /* 3 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x284)},       /* 4 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x285)},       /* 5 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x286)},       /* 6 */
    {UDATA(&mt_srv, UNIT_MT(1), 0), 0, UNIT_ADDR(0x287)},       /* 7 */
};

struct dib mtb_dib = { 0xF8, NUM_UNITS_MT, mt_startio, mt_startcmd, NULL, mtb_unit, mt_ini};

DEVICE              mtb_dev = {
    "MTB", mtb_unit, NULL, mt_mod,
    NUM_UNITS_MT, 8, 15, 1, 8, 8,
    NULL, NULL, NULL, &mt_boot, &mt_attach, &mt_detach,
    &mtb_dib, DEV_BUF_NUM(1) | DEV_DISABLE | DEV_DIS | DEV_DEBUG | DEV_TAPE, 0,
    dev_debug, NULL, NULL, &mt_help, NULL, NULL, &mt_description
};
#endif

#if defined(CPANEL)
// vars used to simulate the timimg of device to operate a real speed respect wallclock
// applies only when cpanel is visible (cpanel_on), and on visible on gui device unit (mta[0..7])
uint32 TapeCmd_tm0[8] = {0};            // set to sim_os_msec, checked by mt_svr to simulate wallclock time needed by device to operate
uint32 TapeCmd_msec[8] = {0};           // duration of device operation

const char *MT_cmd_names[] = {
    "???", 
    "RD", "WR", "RDBK",
    "ERG", "WTM", 
    "FSF", "FSR", "BSF", "BSR", 
    "RUN", "REW",
    "SENSE", "MODE", "STATUS"
};

void cpanel_start_tape_cmd(UNIT * uptr, int cmd)
{
    int unit=uptr - mta_unit; 

    // determine tape unit, check it is mta
    if (cpanel_on == 0) return; 
    if ((unit < 0) || (unit > 7)) return; 
    // determine direction of tape for new command

    cmd = cmd & MT_CMDMSK; 

    mt_info[unit].last_cmd_name = MT_cmd_names[0]; 
    mt_info[unit].last_cmd=cmd; 
    switch (cmd & 0xf) {
        case MT_READ:  mt_info[unit].last_cmd_name = MT_cmd_names[1]; break;  // tape going forward
        case MT_WRITE: mt_info[unit].last_cmd_name = MT_cmd_names[2]; break; 
        case MT_RDBK:  mt_info[unit].last_cmd_name = MT_cmd_names[3]; break;  // tape going backward 
        case 0x7:
        case 0xf:
            switch (cmd) {
                case MT_ERG: mt_info[unit].last_cmd_name = MT_cmd_names[4]; break;  // tape going forward
                case MT_WTM: mt_info[unit].last_cmd_name = MT_cmd_names[5]; break;  
                case MT_FSF: mt_info[unit].last_cmd_name = MT_cmd_names[6]; break;  
                case MT_FSR: mt_info[unit].last_cmd_name = MT_cmd_names[7]; break;  
                case MT_BSF: mt_info[unit].last_cmd_name = MT_cmd_names[8]; break;  // tape going backward 
                case MT_BSR: mt_info[unit].last_cmd_name = MT_cmd_names[9]; break;  
                case MT_RUN: mt_info[unit].last_cmd_name = MT_cmd_names[10]; break;  
                case MT_REW: mt_info[unit].last_cmd_name = MT_cmd_names[11]; break;  
            }
            break;
        case MT_SENSE: mt_info[unit].last_cmd_name = MT_cmd_names[12]; return;  // tape does not change direction, command takes no time
        case MT_MODE:  mt_info[unit].last_cmd_name = MT_cmd_names[13]; return;  
        case 0:        mt_info[unit].last_cmd_name = MT_cmd_names[14]; return;  
    }

    TapeCmd_tm0[unit]  = 0;  // start of new command -> init timing values 
    TapeCmd_msec[unit] = 0;  
}

// calc time for tape command on a record of reclen bytes
// if recles > 0, tapedir=1 for fwd, -1 for backwrd
// if reclen=0, tape_dir=1 -> calc time needed for tape mark/erase gap
// if reclen=-1 -> calc time needed for REW
void cpanel_tape_reclen_rw(UNIT * uptr, int reclen, int tape_dir)
{
    int msec, recsize, bDirChanged; 
    int unit=uptr - mta_unit; 
    uint32 tnow = sim_os_msec();  
    extern int PARAM_RWSpeed; 
    extern int PARAM_HeadOpenTime; 
    extern int PARAM_HiSpeedRwdSpeed; 
    extern int PARAM_HiSpeedRwdTrigger; 
    extern int PARAM_HiSpeedRwdEnd; 

    // determine tape unit, check it is mta
    if (cpanel_on == 0) return; 
    if ((unit < 0) || (unit > 7)) return; 
    
    if (TapeCmd_tm0[unit] == 0) {
       // if start of cmd not set (because new command just started or because cleaned by fastMode), then set it again
        TapeCmd_tm0[unit] = tnow;
        TapeCmd_msec[unit] =0;
    } else if (tnow > TapeCmd_tm0[unit] + TapeCmd_msec[unit]) {
        // if start of cmd + duration has passed (because cpu has been stopped in the meanwhile), then set it again
        TapeCmd_tm0[unit] = tnow;
        TapeCmd_msec[unit] =0;
    }

    // check if doing a rew 
    if (reclen == -1) {
        // Calculate time needed. No need to update recsize, numrw, numrec
        mt_info[unit].TapeDir = -1; // tape going backward
        msec=0;
        // check if rew at high speed. Hi Speed rew is done when at least 1500 inches in take reel
        if (mt_info[unit].MediumPos > PARAM_HiSpeedRwdTrigger * 1000) { //n has inches x1000 of tape medium used
            // unload: open r/w head + 1 sec to remove tape medium from vaccol
            msec += PARAM_HeadOpenTime + 1000; 
            // pause 0.5 sec
            msec += 500; 
            // rewind at 500 inch/sec (average) until 400 inches left
            msec += (mt_info[unit].MediumPos - PARAM_HiSpeedRwdEnd*1000) / PARAM_HiSpeedRwdSpeed; 
            // pause 0.5 sec
            msec += 500; 
            // load: close r/w head + 1 sec to remove tape medium from vaccol
            msec += PARAM_HeadOpenTime + 1000; 
            // rew 400 inches at low speed
            msec += PARAM_HiSpeedRwdEnd*1000 / PARAM_RWSpeed; 
        } else {
            // low speed rewind
            msec += mt_info[unit].MediumPos / PARAM_RWSpeed; 
        }
        TapeCmd_msec[unit]=msec;    
        return; 
    }


    // calculate time (msec) needed to r/w a record of reclen bytes 
    // on a IBM 2401 model 5 or model 2 (when using 7 tracks medium)
    msec=0;

    // When writing or reading from load point, add 75 msec
    if (mt_info[unit].numrec==0) msec+=75; 

    // tape needs 32 msec to change the working direction
    if (mt_info[unit].TapeDir != tape_dir) {
        msec = +32; bDirChanged=1; 
    } else bDirChanged=0;

    // calculate time needed to read/write tape record
    // time table      MT_MDEN_800     MT_MDEN_1600  
    //                  7 tracks        9 tracks
    // interblock gap     0.75            0.6  inches
    //                    10.0            8.0  msec       (at 75 ips)
    // read/write byte    16.6            8.3  usec   
    // tape mark          3.75            3.75 inches
    //                    50              25    msec
  
    if ((uptr->flags & MTUF_9TR) == 0) {
        // using 7 tracks, 800 bpi (bits per inch), 75 ips (inches per second)
        if (reclen==0) { // is a tape mark or ERG
            recsize=3750; // inches x1000
        } else {
            // density is 800 bytes per inch -> each byte needs 0.00125 inches
            // IRG (inter gap record) is 0.75 inch
            // scaled x1000 to use integer values
            recsize = (int32) ((reclen * 0.00125 + 0.75) * 1000);            
        }
    } else {
        // using 9 tracks, 1600 bpi, 75 ips
        if (reclen==0) { // is a tape mark or ERG
            recsize=3750; // inches x1000
        } else {
            // density is 1600 bytes per inch -> each byte needs 0.000625 inches
            // IRG (inter gap record) is 0.6 inch
            // scaled x1000 to use integer values
            recsize = (int32) ((reclen * 0.000625 + 0.6) * 1000);            
        }
    }
    // tape speed is 75 inch per sec (PARAM_RWSpeed) -> 1000/75 = 13.3 msec per inch
    // recsize is inch x1000, -> time needed by recsize is (1000/75)*(recsize/1000) -> recsize/75
    msec = (recsize / PARAM_RWSpeed); 

    mt_info[unit].TapeDir = tape_dir;
    TapeCmd_msec[unit]+=msec;    
    mt_info[unit].recsize=recsize; // size of record in tape medium
    mt_info[unit].numrw++;         // one more r/w operation performed
    mt_info[unit].numrec+=tape_dir; // count record number
    mt_info[unit].MediumPos+=tape_dir*recsize; // where is r/w head positioned in tape medium from reel

    // set values so Refresh_MagTape can animate a tape record read/write
    if ((mt_info[unit].cmd_tm0==0) || (bDirChanged) || (TapeCmd_msec[unit] > 50)) {
        // no animation in progress or tape changes direction or long tape operation -> schedulle a new animation
        mt_info[unit].cmd_tm0  = TapeCmd_tm0[unit];  // when r/w command starts for tape reel motion animation
        mt_info[unit].cmd_msec = TapeCmd_msec[unit]; // signal tape cmd animation pending to be done
    } else {
        // animation already schedulled on same direction -> increment len
        mt_info[unit].cmd_msec = +TapeCmd_msec[unit]; // signal tape cmd animation pending to be done
    }
    // safety
    if (mt_info[unit].MediumPos < 0) {
        mt_info[unit].MediumPos=0; 
    } else if (mt_info[unit].MediumPos > mt_info[unit].MediumLen * 1000) {
        mt_info[unit].MediumPos = mt_info[unit].MediumLen* 1000; 
    }
}

int cpanel_tape_cmd_in_progress(UNIT * uptr)
{
    extern int cpanel_is_rewinding(int unit);
    uint32 msec; 
    int unit=uptr - mta_unit; 

    // determine tape unit, check it is mta
    if (cpanel_on == 0) return 0; 
    if ((unit < 0) || (unit > 7)) return 0; 

    // check is a rew animation is in progress. 
    // If so, keep waiting until animation finishes
    if (cpanel_is_rewinding(unit)) return 1; 

    // if fastmode or not cmd started yet, return
    if (bFastMode) 
        TapeCmd_tm0[unit] = 0;
    if (TapeCmd_tm0[unit] == 0) return 0; 

    msec = sim_os_msec() - TapeCmd_tm0[unit];
    if (msec < TapeCmd_msec[unit]) return 1; // command not yet finished

    // command finished. 
    return 0;
}

void cpanel_tape_cmd_info(UNIT * uptr, int * addr, int * cmd)
{
    if (uptr == NULL) {
        *addr=*cmd=0;
        return; 
    }

    *addr = GET_UADDR(uptr->CMD);
    *cmd  = uptr->CMD & MT_CMDMSK;
    if (uptr->CMD & MT_CMDREW) {
        // a rew is in progress -> retun MT_REW as current command in execution in tape unit
        *cmd = MT_REW; 
    } else if (uptr->CMD & MT_CMDRUN) {
        // a tape run is in progress -> retun MT_RUN as current command in execution in tape unit
        *cmd = MT_RUN; 
    }
}


#define CP_RWTAPE(reclen, tape_dir)    cpanel_tape_reclen_rw(uptr, reclen, tape_dir)  // time needed by tape command to execute;  
#else
#define CP_RWTAPE(reclen)    {}  // do nothing
#endif

uint8               parity_table[64] = {
    /* 0    1    2    3    4    5    6    7 */
    0000, 0100, 0100, 0000, 0100, 0000, 0000, 0100,
    0100, 0000, 0000, 0100, 0000, 0100, 0100, 0000,
    0100, 0000, 0000, 0100, 0000, 0100, 0100, 0000,
    0000, 0100, 0100, 0000, 0100, 0000, 0000, 0100,
    0100, 0000, 0000, 0100, 0000, 0100, 0100, 0000,
    0000, 0100, 0100, 0000, 0100, 0000, 0000, 0100,
    0000, 0100, 0100, 0000, 0100, 0000, 0000, 0100,
    0100, 0000, 0000, 0100, 0000, 0100, 0100, 0000
};

uint8                  bcd_to_ebcdic[64] = {
     0x40, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
     0xf8, 0xf9, 0xf0, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
     0x7a, 0x61, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
     0xe8, 0xe9, 0xe0, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
     0x60, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
     0xd8, 0xd9, 0xd0, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
     0x50, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
     0xc8, 0xc9, 0xc0, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f};

uint8 bin_to_hex[16] = {'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};

void sim_debug_tape_rec(DEVICE *dptr, char * cmd, uint8 * buf, int reclen, int unit, int pos)
{
    int i,n; 
    char sOut[81];
    uint8 ch; 

    if (sim_deb==0) return;
    sim_debug(DEBUG_POS, dptr, "Position record %d: Data %s (%d bytes), unit %d\n", pos, cmd, reclen, unit);
    memset(sOut, 32, sizeof(sOut)); sOut[80]=0;
    for (i=0; i<reclen; i++) {
        ch = buf[i];
        n  = (i & 15)*3; 
        sOut[n++] = bin_to_hex[(ch >> 4) & 0xF];
        sOut[n++] = bin_to_hex[ch & 0xF];
        sOut[n++] = 32;
        ch = ebcdic_to_ascii[ch];
        if (ch < 32) ch = '.';
        n=16*3+5+(i & 15);
        sOut[n++] = ch;
        sOut[n++] = 0;
        if (((i & 15)==15) || (i==reclen-1)) {
            sim_debug(DEBUG_POS, dptr, "... %s\n", sOut);
            memset(sOut, 32, sizeof(sOut)); sOut[80]=0;
        }
    }
}


uint8  mt_startio(UNIT *uptr) {
    DEVICE         *dptr = find_dev_from_unit(uptr);
    unsigned int    i;

    if (mt_busy[GET_DEV_BUF(dptr->flags)] != 0) {
        sim_debug(DEBUG_CMD, dptr, "busy\n");
        return SNS_BSY;
    }
    if ((uptr->CMD & (MT_CMDREW|MT_CMDRUN)) != 0) {
        sim_debug(DEBUG_CMD, dptr, "rew/run in progress\n");
        return SNS_BSY;
    }
    /* Check if controller is free */
    for (i = 0; i < dptr->numunits; i++) {
       if ((dptr->units[i].CMD & MT_CMDMSK) != 0) {
           uptr->flags |= MT_BUSY;   /* Flag we need to send CUE */
           return SNS_SMS|SNS_BSY;
       }
    }
    sim_debug(DEBUG_CMD, dptr, "start io\n");
    return 0;
}

uint8  mt_startcmd(UNIT *uptr,  uint8 cmd) {
    DEVICE         *dptr = find_dev_from_unit(uptr);
    int            unit = (uptr - dptr->units);
    uint8          f = 0;

    if (mt_busy[GET_DEV_BUF(dptr->flags)] != 0 || (uptr->CMD & MT_CMDMSK) != 0) {
        sim_debug(DEBUG_CMD, dptr, "CMD busy unit=%d %x\n", unit, cmd);
        uptr->flags |= MT_BUSY;
        return SNS_BSY;
    }

    if (uptr->flags & MT_BUSY)
        f = SNS_CTLEND;

    sim_debug(DEBUG_CMD, dptr, "CMD unit=%d %x\n", unit, cmd);
#if defined(CPANEL)
    if ((uptr->CMD & (MT_CMDREW|MT_CMDRUN)) == 0) {
        // set mt_info and TapeCmd_tm0/msec only if not rew in progress
        // if rew/run in progress, any command but sense will return busy
        cpanel_start_tape_cmd(uptr, cmd); // start tape command
    }
#endif

    switch (cmd & 0xF) {
    case 0x7:              /* Tape motion */
    case 0xf:              /* Tape motion */
    case 0x1:              /* Write command */
    case 0x2:              /* Read command */
    case 0xc:              /* Read backward */
         uptr->SNS = 0;
          /* Fall through */

    case 0x4:              /* Sense */
         if ((uptr->CMD & MT_CMDREW) != 0) {
            sim_debug(DEBUG_CMD, dptr, "CMD rewinding unit=%d %x\n", unit, cmd);
            return SNS_BSY;
         }
         if ((uptr->CMD & MT_CMDRUN) != 0) {
             sim_debug(DEBUG_CMD, dptr, "CMD unloading unit=%d %x\n", unit, cmd);
             uptr->SNS |= SNS_INTVENT;
             uptr->flags &= ~MT_BUSY;
             return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK|f;
         }
         if ((uptr->flags & UNIT_ATT) == 0) {
             uptr->SNS |= SNS_INTVENT;
             uptr->flags &= ~MT_BUSY;
             return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK|f;
         }
         uptr->CMD &= ~(MT_CMDMSK);
         uptr->CMD |= cmd & MT_CMDMSK;
         sim_activate(uptr, 10);       /* Start unit off */
         CLR_BUF(uptr);
         uptr->POS = 0;
         uptr->CPOS = 0;
         mt_busy[GET_DEV_BUF(dptr->flags)] = 1;
         if ((cmd & 0x7) == 0x7) {         /* Quick end channel on control */
             uptr->flags &= ~MT_BUSY;
             return SNS_CHNEND|f;
         }
         return 0;

    case 0x3:              /* Control */
    case 0xb:              /* Control */
         uptr->SNS = 0;
         if ((uptr->flags & UNIT_ATT) == 0) {
             uptr->SNS |= SNS_INTVENT;
             uptr->flags &= ~MT_BUSY;
             return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK|f;
         }
         if ((uptr->flags & MTUF_9TR) == 0)  {
             uptr->SNS |= (SNS_7TRACK << 8);
             uptr->CMD |= MT_ODD;
             if ((cmd & 0xc0) == 0xc0) {
                 uptr->flags &= ~MT_BUSY;
                 return SNS_CHNEND|SNS_DEVEND|f;
             }
             switch((cmd >> 3) & 07) {
             case 0:      /* NOP */
             case 1:      /* Diagnostics */
             case 3:
                  uptr->flags &= ~MT_BUSY;
                  return SNS_CHNEND|SNS_DEVEND|f;
             case 2:      /* Reset condition */
                  uptr->CMD &= ~(MT_ODD|MT_TRANS|MT_CONV|MT_MDEN_MSK);
                  uptr->CMD |= (cmd & MT_MDEN_MSK) | MT_ODD | MT_CONV;
                  break;
             case 4:
                  uptr->CMD &= ~(MT_ODD|MT_TRANS|MT_CONV|MT_MDEN_MSK);
                  uptr->CMD |= (cmd & MT_MDEN_MSK);
                  break;
             case 5:
                  uptr->CMD &= ~(MT_ODD|MT_TRANS|MT_CONV|MT_MDEN_MSK);
                  uptr->CMD |= (cmd & MT_MDEN_MSK) | MT_TRANS;
                  break;
             case 6:
                  uptr->CMD &= ~(MT_ODD|MT_TRANS|MT_CONV|MT_MDEN_MSK);
                  uptr->CMD |= (cmd & MT_MDEN_MSK) | MT_ODD;
                  break;
             case 7:
                  uptr->CMD &= ~(MT_ODD|MT_TRANS|MT_CONV|MT_MDEN_MSK);
                  uptr->CMD |= (cmd & MT_MDEN_MSK) | MT_ODD | MT_TRANS;
                  break;
             }
         } else {
             uptr->CMD &= ~MT_MDEN_MSK;
             if (cmd & 0x8)
                 uptr->CMD |= MT_MDEN_800;
             else
                 uptr->CMD |= MT_MDEN_1600;
         }
         uptr->SNS = 0;
         break;

    case 0x0:               /* Status */
         break;

    default:                /* invalid command */
         sim_debug(DEBUG_CMD, dptr, "ERROR: Invalid CMD unit=%d %x\n", unit, cmd);
         uptr->SNS |= SNS_CMDREJ;
         break;
    }
    uptr->flags &= ~MT_BUSY;
    if (uptr->SNS & 0xff)
        return SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK|f;
    return SNS_CHNEND|SNS_DEVEND|f;
}

/* Map simH errors into machine errors */
t_stat mt_error(UNIT * uptr, uint16 addr, t_stat r, DEVICE * dptr)
{
    uint8     flags = SNS_CHNEND|SNS_DEVEND;

    if (uptr->flags & MT_BUSY) {
       flags |= SNS_CTLEND;
       uptr->flags &= ~MT_BUSY;
    }

    mt_busy[GET_DEV_BUF(dptr->flags)] &= ~1;
    switch (r) {
    case MTSE_OK:              /* no error */
       break;

    case MTSE_TMK:              /* tape mark */
       sim_debug(DEBUG_EXP, dptr, "MARK ");
       chan_end(addr, flags|SNS_UNITEXP);
       return SCPE_OK;

    case MTSE_WRP:              /* write protected */
    case MTSE_UNATT:              /* unattached */
       sim_debug(DEBUG_EXP, dptr, "ATTENTION %d ", r);
       break;

    case MTSE_IOERR:              /* IO error */
    case MTSE_FMT:              /* invalid format */
    case MTSE_RECE:              /* error in record */
       sim_debug(DEBUG_EXP, dptr, "ERROR %d ", r);
       break;
    case MTSE_BOT:              /* beginning of tape */
       sim_debug(DEBUG_EXP, dptr, "BOT ");
       break;
    case MTSE_INVRL:              /* invalid rec lnt */
       sim_debug(DEBUG_EXP, dptr, "ERROR: INVALID REC LNT ");
       break;
    case MTSE_EOM:              /* end of medium */
       sim_debug(DEBUG_EXP, dptr, "EOT ");
       chan_end(addr, flags|SNS_UNITEXP);
       return SCPE_OK;
    }
    chan_end(addr, flags);
    return SCPE_OK;
}

/* Handle processing of tape requests. */
t_stat mt_srv(UNIT * uptr)
{
    uint16              addr = GET_UADDR(uptr->CMD);
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 cmd = uptr->CMD & MT_CMDMSK;
    int                 bufnum = GET_DEV_BUF(dptr->flags);
    t_mtrlnt            reclen;
    t_stat              r = SCPE_ARG;       /* Force error if not set */
    uint8               ch;
    int                 mode = 0;

#if defined(CPANEL)
    if (cpanel_tape_cmd_in_progress(uptr)) {
        // operation still in progress ... check again later
        sim_activate(uptr, check_later_interval);       
        return SCPE_OK;
    }
#endif

    if ((uptr->flags & UNIT_ATT) == 0) {
        uptr->SNS |= SNS_INTVENT;
        if (cmd != MT_SENSE) {
            uptr->CMD &= ~(MT_CMDMSK);
            mt_busy[bufnum] &= ~1;
            if (uptr->flags & MT_BUSY) {
               uptr->flags &= ~MT_BUSY;
               chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK);
            } else {
               chan_end(addr, SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK);
            }
            return SCPE_OK;
        }
    }

    if ((uptr->CMD & MT_CMDREW) != 0) {
        sim_debug(DEBUG_DETAIL, dptr, "Rewind unit=%d\n done", unit);
        uptr->CMD &= ~(MT_CMDREW);
        set_devattn(addr, SNS_DEVEND); // second DEVEND issued on rew end 
        return SCPE_OK;
    }
    if ((uptr->CMD & MT_CMDRUN) != 0) {
        sim_debug(DEBUG_DETAIL, dptr, "Unload unit=%d done\n", unit);
        uptr->CMD &= ~(MT_CMDRUN);
        sim_tape_detach(uptr);
        return SCPE_OK;
    }

    switch (cmd & 0xf) {
    case 0:                               /* No command, stop tape */
         sim_debug(DEBUG_DETAIL, dptr, "Idle unit=%d\n", unit);
         break;

    case MT_SENSE:
         ch = uptr->SNS & 0xff;
         sim_debug(DEBUG_DETAIL, dptr, "sense unit=%d 1 %x\n", unit, ch);
         chan_write_byte(addr, &ch) ;
         ch = (uptr->SNS >> 8) & 0xff;
         if ((uptr->flags & MTUF_9TR) == 0)
             ch |= SNS_7TRACK;
         if ((uptr->flags & UNIT_ATT) != 0) {
             if (sim_tape_wrp(uptr))
                 ch |= SNS_WRP;
             if (sim_tape_bot(uptr))
                 ch |= SNS_LOAD;
             ch |= SNS_TUASTA;
         }
         sim_debug(DEBUG_DETAIL, dptr, "sense unit=%d 2 %x\n", unit, ch);
         chan_write_byte(addr, &ch) ;
         ch = SNS_BYTE2;
         sim_debug(DEBUG_DETAIL, dptr, "sense unit=%d 3 %x\n", unit, ch);
         chan_write_byte(addr, &ch) ;
         ch = (uptr->SNS >> 16) & 0xff;
         if ((uptr->flags & MTUF_9TR) != 0)
            ch |= 04;
         sim_debug(DEBUG_DETAIL, dptr, "sense unit=%d 4 %x\n", unit, ch);
         chan_write_byte(addr, &ch) ;
         ch = SNS_BYTE4;
         chan_write_byte(addr, &ch) ;
         ch = SNS_BYTE5;
         chan_write_byte(addr, &ch);
         uptr->CMD &= ~MT_CMDMSK;
         mt_busy[bufnum] &= ~1;
         if (uptr->flags & MT_BUSY) {
            uptr->flags &= ~MT_BUSY;
            chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
         } else {
            chan_end(addr, SNS_CHNEND|SNS_DEVEND);
         }
         break;

    case MT_READ:

         if (uptr->CMD & MT_READDONE) {
            uptr->CMD &= ~(MT_CMDMSK|MT_READDONE);
            mt_busy[bufnum] &= ~1;
            if (uptr->flags & MT_BUSY) {
               uptr->flags &= ~MT_BUSY;
               chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
            } else {
               chan_end(addr, SNS_CHNEND|SNS_DEVEND);
            }
            break;
         }

         /* If empty buffer, fill */
         if (BUF_EMPTY(uptr)) {
             sim_debug(DEBUG_DETAIL, dptr, "Read unit=%d ", unit);
             if ((r = sim_tape_rdrecf(uptr, &mt_buffer[bufnum][0], &reclen,
                              BUFFSIZE)) != MTSE_OK) {
                 sim_debug(DEBUG_DETAIL, dptr, " error %d\n", r);
                 uptr->CMD &= ~(MT_CMDMSK|MT_READDONE);
                 return mt_error(uptr, addr, r, dptr);
             }
             uptr->POS = 0;
             uptr->CPOS = 0;
             uptr->hwmark = reclen;
             sim_debug(DEBUG_DETAIL, dptr, "Block %d chars\n", reclen);
             sim_debug_tape_rec(dptr, "read", &mt_buffer[bufnum][0], reclen, unit, uptr->POS);
             CP_RWTAPE(reclen,1); // time needed by tape command to execute
         }

         ch = mt_buffer[bufnum][uptr->POS++];
         /* if we are a 7track tape, handle conversion */
         if ((uptr->flags & MTUF_9TR) == 0) {
             mode = (uptr->CMD & MT_ODD) ? 0 : 0100;
             if ((parity_table[ch & 077] ^ (ch & 0100) ^ mode) == 0) {
                 sim_debug(DEBUG_DETAIL, dptr, "Parity error unit=%d %d %03o\n",
                       unit, uptr->POS-1, ch);
                 uptr->SNS |= (SNS_VRC << 16) | SNS_DATCHK;
             }
             ch &= 077;
             if (uptr->CMD & MT_TRANS)
                 ch = bcd_to_ebcdic[ch];
             if (uptr->CMD & MT_CONV) {
                 sim_debug(DEBUG_DATA, dptr, "Read raw data unit=%d %d %02x %02x\n",
                       unit, uptr->POS, ch, uptr->CPOS);
                 if (uptr->CPOS == 0 && (t_addr)uptr->POS < uptr->hwmark) {
                     uptr->CPOS = MT_CONV1 | ch;
                     sim_activate(uptr, 10);
                     return SCPE_OK;
                 } else if ((uptr->CPOS & 0xc0) == MT_CONV1) {
                     int t = uptr->CPOS & 0x3F;
                     uptr->CPOS = MT_CONV2 | ch;
                     ch = (t << 2) | ((ch >> 4) & 03);
                 } else if ((uptr->CPOS & 0xc0) == MT_CONV2) {
                     int  t = uptr->CPOS & 0xf;
                     uptr->CPOS = MT_CONV3 | ch;
                     ch = (t << 4) | ((ch >> 2) & 0xf);
                 } else if ((uptr->CPOS & 0xc0) == MT_CONV3) {
                     ch |= ((uptr->CPOS & 0x3) << 6);
                     uptr->CPOS = 0;
                 }
             }
         }

         /* Send character over to channel */
         if (chan_write_byte(addr, &ch)) {
             sim_debug(DEBUG_DATA, dptr, "Read unit=%d EOR\r\n", unit);
             /* If not read whole record, skip till end */
             if ((t_addr)uptr->POS < uptr->hwmark) {
                 /* Send dummy character to force SLI */
                 chan_write_byte(addr, &ch);
                 #if defined(CPANEL)
                 sim_activate(uptr, 10);
                 #else
                 sim_activate(uptr, (uptr->hwmark-uptr->POS) * 10);
                 #endif
                 uptr->CMD |= MT_READDONE;
                 break;
             }
             uptr->CMD &= ~MT_CMDMSK;
             mt_busy[bufnum] &= ~1;
             if (uptr->flags & MT_BUSY) {
                uptr->flags &= ~MT_BUSY;
                chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
             } else {
                chan_end(addr, SNS_CHNEND|SNS_DEVEND);
             }
         } else {
             char c = ebcdic_to_ascii[ch];
             sim_debug(DEBUG_DATA, dptr, "Read data unit=%d %d %02x (ascii %d %c)\r\n",
                       unit, uptr->POS, ch, c, (c<32)?32:c);
              if ((t_addr)uptr->POS >= uptr->hwmark) {       /* In IRG */
                  /* Handle end of record */
                  uptr->CMD &= ~MT_CMDMSK;
                  mt_busy[bufnum] &= ~1;
                  if (uptr->flags & MT_BUSY) {
                     uptr->flags &= ~MT_BUSY;
                     chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
                  } else {
                     chan_end(addr, SNS_CHNEND|SNS_DEVEND);
                  }
             } else
                  sim_activate(uptr, 10);
         }
         break;


    case MT_WRITE:
         /* Check if write protected */
         if (sim_tape_wrp(uptr)) {
             uptr->SNS |= SNS_CMDREJ;
             uptr->CMD &= ~MT_CMDMSK;
             mt_busy[bufnum] &= ~1;
             if (uptr->flags & MT_BUSY) {
                uptr->flags &= ~MT_BUSY;
                chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
             } else {
                chan_end(addr, SNS_CHNEND|SNS_DEVEND);
             }
             break;
         }

         /* Grab data until channel has no more */
         if (chan_read_byte(addr, &ch)) {
             if (uptr->POS > 0 || uptr->CPOS != 0) {/* Only if data in record */
                 if ((uptr->flags & MTUF_9TR) == 0) {
                     mode = (uptr->CMD & MT_ODD) ? 0100 : 0;
                     if (uptr->CMD & MT_CONV) {
                         if ((uptr->CPOS & 0xc0) == MT_CONV1) {
                             int t = (uptr->CPOS & 0x3) << 4;
                             t ^= parity_table[t & 077] ^ mode;
                             mt_buffer[bufnum][uptr->POS++] = t;
                        } else if ((uptr->CPOS & 0xc0) == MT_CONV2) { 
                             int  t = (uptr->CPOS & 0xf) << 2;
                             t ^= parity_table[t & 077] ^ mode;
                             mt_buffer[bufnum][uptr->POS++] = t;
                        }
                        uptr->hwmark = uptr->POS;
                    }
                 }
                 reclen = uptr->hwmark;
                 sim_debug(DEBUG_DETAIL, dptr, "Write unit=%d Block %d chars\n",
                          unit, reclen);
                 r = sim_tape_wrrecf(uptr, &mt_buffer[bufnum][0], reclen);
                 uptr->POS = 0;
                 uptr->CMD &= ~MT_CMDMSK;
                 mt_error(uptr, addr, r, dptr);       /* Record errors */
                 CP_RWTAPE(reclen,1); // time needed by tape command to execute
             } else {
                 uptr->SNS |= SNS_WCZERO;              /* Write with no data */
             }
         } else {
             if ((uptr->flags & MTUF_9TR) == 0) {
                 mode = (uptr->CMD & MT_ODD) ? 0100 : 0;
                 if (uptr->CMD & MT_TRANS)
                     ch = (ch & 0xf) | ((ch & 0x30) ^ 0x30);
                 if (uptr->CMD & MT_CONV) {
                     if (uptr->CPOS == 0) {
                         uptr->CPOS = MT_CONV1 | (ch & 0x3);
                         ch >>= 2;
                     } else if ((uptr->CPOS & 0xc0) == MT_CONV1) {
                         int t = uptr->CPOS & 0x3;
                         uptr->CPOS = MT_CONV2 | (ch & 0xf);
                         ch = (t << 4) | ((ch >> 4) & 0xf);
                    } else if ((uptr->CPOS & 0xc0) == MT_CONV2) {
                         int  t = uptr->CPOS & 0xf;
                         t = (t << 2) | ((ch >> 6) & 0x3);
                         t ^= parity_table[t & 077] ^ mode;
                         mt_buffer[bufnum][uptr->POS++] = t;
                         uptr->CPOS = 0;
                    }
                }
                ch &= 077;
                ch |= parity_table[ch] ^ mode;
             }
             mt_buffer[bufnum][uptr->POS++] = ch;
             sim_debug(DEBUG_DATA, dptr, "Write data unit=%d %d %02o\r\n",
                      unit, uptr->POS, ch);
             uptr->hwmark = uptr->POS;
         }
         sim_activate(uptr, 10);
         break;

    case MT_RDBK:
         if (uptr->CMD & MT_READDONE) {
            uptr->CMD &= ~(MT_CMDMSK|MT_READDONE);
            mt_busy[bufnum] &= ~1;
            if (uptr->flags & MT_BUSY) {
               uptr->flags &= ~MT_BUSY;
               chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
            } else {
               chan_end(addr, SNS_CHNEND|SNS_DEVEND);
            }
            return SCPE_OK;
         }

         /* If at end of record, fill buffer */
         if (BUF_EMPTY(uptr)) {
              if (sim_tape_bot(uptr)) {
                  uptr->CMD &= ~MT_CMDMSK;
                  mt_busy[GET_DEV_BUF(dptr->flags)] &= ~1;
                  if (uptr->flags & MT_BUSY) {
                     uptr->flags &= ~MT_BUSY;
                     chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK);
                  } else {
                     chan_end(addr, SNS_CHNEND|SNS_DEVEND|SNS_UNITCHK);
                  }
                  return SCPE_OK;
              }
             sim_debug(DEBUG_DETAIL, dptr, "Read backward unit=%d ", unit);
             if ((r = sim_tape_rdrecr(uptr, &mt_buffer[bufnum][0], &reclen,
                                BUFFSIZE)) != MTSE_OK) {
                  uptr->CMD &= ~(MT_CMDMSK|MT_READDONE);
                  return mt_error(uptr, addr, r, dptr);
             }
             sim_debug_tape_rec(dptr, "read backward", &mt_buffer[bufnum][0], reclen, unit, uptr->POS);
             uptr->POS = reclen;
             uptr->hwmark = reclen;
             sim_debug(DEBUG_DETAIL, dptr, "Binary Block %d chars\n", reclen);
             CP_RWTAPE(reclen,-1); // time needed by tape command to execute
         }

         ch = mt_buffer[bufnum][--uptr->POS];
         if ((uptr->flags & MTUF_9TR) == 0) {
             mode = (uptr->CMD & MT_ODD) ? 0 : 0100;
             ch &= 077;
             if ((parity_table[ch & 077] ^ (ch & 0100) ^ mode) == 0) {
                 uptr->SNS |= (SNS_VRC << 16) | SNS_DATCHK;
             }
             if (uptr->CMD & MT_TRANS)
                 ch = bcd_to_ebcdic[ch];
         }

         if (chan_write_byte(addr, &ch)) {
                   sim_debug(DEBUG_DATA, dptr, "Read unit=%d EOR\r\n", unit);
              /* If not read whole record, skip till end */
              if (uptr->POS >= 0) {
                  #if defined(CPANEL)
                  sim_activate(uptr, 10);
                  #else
                  sim_activate(uptr, (uptr->POS) * 20);
                  #endif
                  uptr->CMD |= MT_READDONE;
                  return SCPE_OK;
              }
              uptr->CMD &= ~MT_CMDMSK;
              mt_busy[bufnum] &= ~1;
              if (uptr->flags & MT_BUSY) {
                 uptr->flags &= ~MT_BUSY;
                 chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
              } else {
                 chan_end(addr, SNS_CHNEND|SNS_DEVEND);
              }
         } else {
              sim_debug(DEBUG_DATA, dptr, "Read data unit=%d %d %02o\r\n",
                            unit, uptr->POS, ch);
              if (uptr->POS == 0) {      /* In IRG */
                  uptr->CMD &= ~MT_CMDMSK;
                  mt_busy[bufnum] &= ~1;
                  if (uptr->flags & MT_BUSY) {
                     uptr->flags &= ~MT_BUSY;
                     chan_end(addr, SNS_CTLEND|SNS_CHNEND|SNS_DEVEND);
                  } else {
                     chan_end(addr, SNS_CHNEND|SNS_DEVEND);
                  }
               } else
                  sim_activate(uptr, 10);
         }
         break;
    case 0x7:
    case 0xf:
         switch (cmd) {
         case MT_WTM:
              if (uptr->POS == 0) {
                 if (sim_tape_wrp(uptr)) {
                     sim_debug(DEBUG_CMD, dptr, "ERROR: writing Tape Mark to read only tape\n");
                     uptr->SNS |= SNS_CMDREJ;
                     uptr->CMD &= ~MT_CMDMSK;
                     mt_busy[GET_DEV_BUF(dptr->flags)] &= ~1;
                     uptr->flags &= ~MT_BUSY;
                     set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                     return SCPE_OK;
                 }
                 uptr->POS ++;
                 #if defined(CPANEL)
                 sim_activate(uptr, 10);
                 CP_RWTAPE(0,1); // time needed by WTM tape command to execute
                 #else
                 sim_activate(uptr, 500);
                 #endif
              } else {
                 sim_debug(DEBUG_DETAIL, dptr, "Write Mark unit=%d\n", unit);
                 uptr->CMD &= ~(MT_CMDMSK);
                 r = sim_tape_wrtmk(uptr);
                 uptr->flags &= ~MT_BUSY;
                 set_devattn(addr, SNS_DEVEND);
                 mt_busy[bufnum] &= ~1;
              }
              break;

         case MT_BSR:
              switch (uptr->POS ) {
              case 0:
                   if (sim_tape_bot(uptr)) {
                       uptr->CMD &= ~MT_CMDMSK;
                       mt_busy[GET_DEV_BUF(dptr->flags)] &= ~1;
                       uptr->flags &= ~MT_BUSY;
                       set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                       return SCPE_OK;
                   }
                   uptr->POS ++;
                   #if defined(CPANEL)
                   sim_activate(uptr, 10);
                   #else
                   sim_activate(uptr, 500);
                   #endif
                   break;
              case 1:
                   uptr->POS++;
                   r = sim_tape_sprecr(uptr, &reclen);
                   sim_debug(DEBUG_DETAIL, dptr, "Backspace rec unit=%d %d ",
                           unit, reclen);
                   /* We don't set EOF on BSR */
                   if (r == MTSE_TMK) {
                       uptr->POS++;
                       sim_debug(DEBUG_DETAIL, dptr, "MARK\n");
                       sim_activate(uptr, 10);
                   } else {
                       sim_debug(DEBUG_DETAIL, dptr, "%d \n", reclen);
                       #if defined(CPANEL)
                       sim_activate(uptr, 10);
                       CP_RWTAPE(reclen,-1); // time needed by tape command to execute
                       #else
                       sim_activate(uptr, 10 + (10 * reclen));
                       #endif
                   }
                   break;
              case 2:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND);
                   mt_busy[bufnum] &= ~1;
                   break;
              case 3:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND|SNS_UNITEXP);
                   mt_busy[bufnum] &= ~1;
                   break;
              }
              break;

         case MT_BSF:
              switch(uptr->POS) {
              case 0:
                   if (sim_tape_bot(uptr)) {
                       uptr->CMD &= ~MT_CMDMSK;
                       mt_busy[bufnum] &= ~1;
                       uptr->flags &= ~MT_BUSY;
                       set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                       break;
                    }
                    uptr->POS ++;
                    #if defined(CPANEL)
                    sim_activate(uptr, 10);
                    #else
                    sim_activate(uptr, 500);
                    #endif
                    break;
              case 1:
                   r = sim_tape_sprecr(uptr, &reclen);
                   sim_debug(DEBUG_DETAIL, dptr, "Backspace file unit=%d %d\n",
                            unit, reclen);
                   if (r == MTSE_TMK) {
                       uptr->POS++;
                       sim_debug(DEBUG_DETAIL, dptr, "MARK\n");
                       sim_activate(uptr, 10);
                    } else if (r == MTSE_BOT) {
                       uptr->POS+= 2;
                       sim_activate(uptr, 10);
                    } else {
                       #if defined(CPANEL)
                       sim_activate(uptr, 10);
                       CP_RWTAPE(reclen,-1); // time needed by tape command to execute
                       #else
                       sim_activate(uptr, 10 + (10 * reclen));
                       #endif
                    }
                    break;
              case 2:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND);
                   mt_busy[bufnum] &= ~1;
                   break;
              case 3:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                   mt_busy[bufnum] &= ~1;
                   break;
              }
              break;

         case MT_FSR:
              switch(uptr->POS) {
              case 0:
                   uptr->POS ++;
                   sim_activate(uptr, 10);
                   break;
              case 1:
                   uptr->POS++;
                   r = sim_tape_sprecf(uptr, &reclen);
                   sim_debug(DEBUG_DETAIL, dptr, "Skip rec unit=%d %d ", unit, reclen);
                   if (r == MTSE_TMK) {
                       uptr->POS = 3;
                       sim_debug(DEBUG_DETAIL, dptr, "MARK\n");
                       sim_activate(uptr, 10);
                   } else if (r == MTSE_EOM) {
                       uptr->POS = 2;
                       sim_activate(uptr, 10);
                   } else {
                       sim_debug(DEBUG_DETAIL, dptr, "%d\n", reclen);
                       #if defined(CPANEL)
                       sim_activate(uptr, 10);
                       CP_RWTAPE(reclen,1); // time needed by tape command to execute
                       #else
                       sim_activate(uptr, 10 + (10 * reclen));
                       #endif
                   }
                   break;
              case 2:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND);
                   mt_busy[bufnum] &= ~1;
                   break;
              case 3:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND|SNS_UNITEXP);
                   mt_busy[bufnum] &= ~1;
                   break;
              case 4:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                   mt_busy[bufnum] &= ~1;
                   break;
              }
              break;

         case MT_FSF:
              switch(uptr->POS) {
              case 0:
                   uptr->POS ++;
                   sim_activate(uptr, 10);
                   break;
              case 1:
                   r = sim_tape_sprecf(uptr, &reclen);
                   sim_debug(DEBUG_DETAIL, dptr, "Skip frec unit=%d %d ", unit, reclen);
                   if (r == MTSE_TMK) {
                       uptr->POS++;
                       sim_debug(DEBUG_DETAIL, dptr, "MARK\n");
                       sim_activate(uptr, 10);
                   } else if (r == MTSE_EOM) {
                       uptr->POS+= 2;
                       sim_activate(uptr, 10);
                   } else {
                       sim_debug(DEBUG_DETAIL, dptr, "%d\n", reclen);
                       #if defined(CPANEL)
                       sim_activate(uptr, 10);
                       CP_RWTAPE(reclen,1); // time needed by tape command to execute
                       #else
                       sim_activate(uptr, 10 + (10 * reclen));
                       #endif
                   }
                   break;
              case 2:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND);
                   mt_busy[bufnum] &= ~1;
                   sim_debug(DEBUG_DETAIL, dptr, "Skip done unit=%d\n", unit);
                   break;
              case 3:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                   mt_busy[bufnum] &= ~1;
                   break;
              }
              break;

         case MT_ERG:
              switch (uptr->POS) {
              case 0:
                   if (sim_tape_wrp(uptr)) {
                       sim_debug(DEBUG_CMD, dptr, "ERROR: erase gap in read only tape\n");
                       uptr->SNS |= SNS_CMDREJ;
                       uptr->CMD &= ~MT_CMDMSK;
                       mt_busy[bufnum] &= ~1;
                       uptr->flags &= ~MT_BUSY;
                       set_devattn(addr, SNS_DEVEND|SNS_UNITCHK);
                   } else {
                       uptr->POS ++;
                       sim_activate(uptr, 10);
                   }
                   break;
              case 1:
                   sim_debug(DEBUG_DETAIL, dptr, "Erase unit=%d\n", unit);
                   r = sim_tape_wrgap(uptr, 35);
                   #if defined(CPANEL)
                   sim_activate(uptr, 10);
                   CP_RWTAPE(0,1); // time needed by ERG cmd to execute
                   #else
                   sim_activate(uptr, 5000);
                   #endif
                   uptr->POS++;
                   break;
              case 2:
                   uptr->CMD &= ~(MT_CMDMSK);
                   uptr->flags &= ~MT_BUSY;
                   set_devattn(addr, SNS_DEVEND);
                   mt_busy[bufnum] &= ~1;
              }
              break;

         case MT_REW:
              mt_busy[bufnum] &= ~1;
              uptr->CMD &= ~(MT_CMDMSK);
              uptr->CMD |= MT_CMDREW;
#if defined(CPANEL)
              // calculate time needed to perform rew
              CP_RWTAPE(-1, 0); // time needed by REW
              sim_debug(DEBUG_DETAIL, dptr, "Rewind unit=%d started. Will take %d seconds\n", unit, TapeCmd_msec[unit] /1000);
              {
                int unit=uptr - mta_unit; 
                if ((unit >= 0) && (unit < 8)) {
                    mt_info[unit].recsize = mt_info[unit].MediumPos;
                    mt_info[unit].MediumPos = 0;
                    mt_info[unit].numrec = 0;
                }
              }
              sim_activate(uptr, 10);
#else
              sim_activate(uptr, 1000 + (20 * uptr->pos));
#endif
              sim_debug(DEBUG_DETAIL, dptr, "Rewind unit=%d\n started", unit);
              r = sim_tape_rewind(uptr);
              set_devattn(addr, SNS_DEVEND); // first DEVEND issued on rew start
              break;
         case MT_RUN:
              mt_busy[bufnum] &= ~1;
              uptr->CMD &= ~(MT_CMDMSK);
              uptr->CMD |= MT_CMDRUN;
#if defined(CPANEL)
              sim_activate(uptr, 10000); // allow 1k cycles to be exec before detaching the file from unit
                                         // during this time, any command to tape but sense will return BUSY
#else
              sim_activate(uptr, 1000 + (20 * uptr->pos));
#endif
              set_devattn(addr, SNS_DEVEND);
              sim_debug(DEBUG_DETAIL, dptr, "Unload unit=%d\n started", unit);
              break;
         }
    }
    return SCPE_OK;
}

void
mt_ini(UNIT * uptr, t_bool f)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);

    uptr->CMD &= UNIT_ADDR_MASK;
    if ((uptr->flags & MTUF_9TR) == 0)
        uptr->CMD |= MT_ODD|MT_CONV|MT_MDEN_800;
    mt_busy[GET_DEV_BUF(dptr->flags)] = 0;
#if defined(CPANEL)
    {
        int unit=uptr - mta_unit; 
        if ((unit >= 0) && (unit < 8)) {
            mt_info[unit].MediumPos = 0;
            mt_info[unit].MediumLen = 28800; // default 2400 ft reel; 1 foot = 12 inches; 2400 ft = 28800 inches
            mt_info[unit].numrec = mt_info[unit].numrw = 0;
            mt_info[unit].TapeDir = 0;
        }
    }
#endif
}

t_stat
mt_boot(int32 unit_num, DEVICE * dptr)
{
    UNIT               *uptr = &dptr->units[unit_num];

    if ((uptr->flags & UNIT_ATT) == 0)
        return SCPE_UNATT;      /* attached? */
    if ((uptr->flags & MTUF_9TR) == 0)  {
        uptr->CMD &= UNIT_ADDR_MASK;
        uptr->CMD |= MT_ODD|MT_CONV|MT_MDEN_800;
    }
    return chan_boot(GET_UADDR(uptr->CMD), dptr);
}

t_stat
mt_attach(UNIT * uptr, CONST char *file)
{
    uint16              addr = GET_UADDR(uptr->CMD);
    t_stat              r;

    uptr->flags &= ~UNIT_RO; // remove readonly flag
    if ((r = sim_tape_attach_ex(uptr, file, 0, 0)) != SCPE_OK)
       return r;
    set_devattn(addr, SNS_DEVEND);
    uptr->CMD &= UNIT_ADDR_MASK;
    if ((uptr->flags & MTUF_9TR) == 0)  {
        uptr->CMD |= MT_ODD | MT_CONV | MT_MDEN_800;
    }
    uptr->POS = 0;
    uptr->SNS = 0;
#if defined(CPANEL)
    {
        int unit=uptr - mta_unit; 
        if ((unit >= 0) && (unit < 8)) {
            mt_info[unit].justattached = 1; 
            mt_info[unit].justdetached = 0; 
            mt_info[unit].MediumPos = 0;
            mt_info[unit].TapeDir = 0;
            mt_info[unit].numrw = 0;
            mt_info[unit].numrec = 0;
            mt_info[unit].recsize = 0;
        }
    }
#endif
    return SCPE_OK;
}

t_stat
mt_detach(UNIT * uptr)
{
    uptr->CMD &= UNIT_ADDR_MASK;
    uptr->POS = 0;
    uptr->SNS = 0;
#if defined(CPANEL)
    {
        int unit=uptr - mta_unit; 
        if ((unit >= 0) && (unit < 8)) {
            mt_info[unit].justattached = 0; 
            mt_info[unit].justdetached = 1; 
            mt_info[unit].recsize = mt_info[unit].MediumPos;
            mt_info[unit].MediumPos = 0;
            mt_info[unit].numrec = 0;
        }
    }
#endif
    return sim_tape_detach(uptr);
}

#if defined(CPANEL)

/* Rewind tape drive */
t_stat mt_rew(UNIT * uptr, int32 val, CONST char *cptr, void *desc)
{
    uint16              addr = GET_UADDR(uptr->CMD);
    int unit=uptr - mta_unit; 

    /* If drive is offline or not attached return not ready */
    if ((uptr->flags & UNIT_ATT) == 0) return SCPE_UNATT;
    set_devattn(addr, SNS_DEVEND);
    uptr->CMD &= UNIT_ADDR_MASK;
    uptr->POS = 0;
    uptr->SNS = 0;
    if ((unit >= 0) && (unit < 8)) {
        mt_info[unit].MediumPos = 0;
        mt_info[unit].numrec = 0;
    }
    return sim_tape_rewind(uptr);
}

/* Set tape length */

t_stat mt_set_len (UNIT *uptr, int32 val, CONST char *cptr, void *desc)
{
    int len;
    t_stat r;
    int unit=uptr - mta_unit; 

    if ((cptr == NULL) || (*cptr == 0))  return SCPE_ARG;
    len = (int) get_uint (cptr, 10, 10000, &r);
    if (r != SCPE_OK) return SCPE_ARG;
    if (len < 50) return SCPE_ARG;
    if ((unit >= 0) && (unit < 8)) {
        mt_info[unit].MediumLen = 28800 * len / 2400;
    } else {
        sim_printf("Length can only be set for MTA units\r\n");
    }
    return SCPE_OK;
}

/* Show tape length */

t_stat mt_show_len (FILE *st, UNIT *uptr, int32 val, CONST void *desc)
{
    int unit=uptr - mta_unit; 
    if ((unit >= 0) && (unit < 8)) {
        fprintf (st, "length %d feet", mt_info[unit].MediumLen * 2400 / 28800);
    } else {
        fprintf (st, "");
    }
    return SCPE_OK;
}

t_stat mt_set_cabinet_door(UNIT * uptr, int32 val, CONST char *cptr,void *desc)
{
    extern void cpanel_set_mt_door(int unit, int state);
    int unit=uptr - mta_unit; 
    if ((unit >= 0) && (unit < 8)) {
        cpanel_set_mt_door(unit, val); 
    }
    return SCPE_OK;
}

#endif

t_stat mt_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
fprintf (st, "2400 Magnetic Tape\n\n");
fprint_set_help (st, dptr);
fprint_show_help (st, dptr);
fprintf (st, "\nThe type options can be used only when a unit is not attached to a file.  The\n");
fprintf (st, "bad block option can be used only when a unit is attached to a file.\n");
fprintf (st, "The magtape supports the BOOT command.\n");
sim_tape_attach_help (st, dptr, uptr, flag, cptr);
return SCPE_OK;
}

const char *mt_description (DEVICE *dptr)
{
return "2400 magnetic tape" ;
}

#endif
