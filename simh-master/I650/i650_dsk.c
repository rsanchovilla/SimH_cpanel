/* i650_dsk.c: IBM 650 RAMAC Disk Dotrage

   Copyright (c) 2018-2020, Roberto Sancho

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
   ROBERTO SANCHO BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   
*/

#include "i650_defs.h"

#define UNIT_DSK       UNIT_ATTABLE | UNIT_DISABLE | UNIT_FIX

#define DISK_SIZE     (12*60*100)   // a physical disk plate size: 12 bytes per word x 60 words per track x 100 tracks per disk
                                    // there are 100 like this in each unit

#define UPDATE_RAMAC        10      // update ramac arm movement each 10 msec of simulted time
                                    // time pregress as drum wordcount progresses

extern uint8   StopIOError;         // flag to signal device that caused STOP_IO error: 1=CDR/CDP, 2=TAPE, 3=RAMAC Address, 4=RAMAC IO
extern int CpuSpeed_Acceleration;   // cpu speed multiplier

/* Definitions */
t_stat              dsk_cmd(int opcode, int addr, int * total_msec, int fast);
t_stat              dsk_srv(UNIT *);
void                dsk_ini(UNIT *, t_bool f);
t_stat              dsk_reset(DEVICE *);
t_stat              dsk_attach(UNIT *, CONST char *);
t_stat              dsk_detach(UNIT *);
t_stat              dsk_help (FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr);
const char          *dsk_description (DEVICE *dptr);

UNIT                dsk_unit[4] = {
    {UDATA(&dsk_srv, UNIT_DSK, 0), 0}, /* 0 */
    {UDATA(&dsk_srv, UNIT_DSK, 0), 0}, /* 1 */
    {UDATA(&dsk_srv, UNIT_DSK, 0), 0}, /* 2 */
    {UDATA(&dsk_srv, UNIT_DSK, 0), 0}, /* 3 */
};

DEVICE              dsk_dev = {
    "DSK", dsk_unit, NULL, NULL,
    4, 8, 15, 1, 8, 8,
    NULL, NULL, &dsk_reset, NULL, &dsk_attach, &dsk_detach,
    &dsk_dib, DEV_DISABLE | DEV_DEBUG, 0, dev_debug,
    NULL, NULL, &dsk_help, NULL, NULL, &dsk_description
};

// this array holds arm position for each disk unit
armrec Arm[4][3];

int LastDiskCmd       = 0;    // last disk command executed (last 2 digits) + IAS being accedes (third digit)

int dsk_read_numeric_word(char * buf, t_int64 * d, int * ZeroNeg)
{
    int i, neg; 
    char c;

    neg = 0;
    *d = 0;
    if (ZeroNeg != NULL) *ZeroNeg = 0;
    for (i=0;i<10;i++) {
        c = *buf++;
        if ((c < '0') || (c > '9')) c='0';
        *d = *d * 10 + (c - '0');
    }
    if (*buf++ == '-') neg=1;
    if (neg) *d = -*d;
    if (ZeroNeg != NULL) *ZeroNeg = ((neg) && (*d == 0)) ? 1:0;
    return 0;
}



void dsk_write_numeric_word(char * buf, t_int64 d, int ZeroNeg)
{
    int i, neg; 
    char c;

    neg = 0;
    if (d < 0) {neg=1; d=-d;}
    if (ZeroNeg) neg=1;
    for (i=0;i<10;i++) {
        c = Shift_Digits(&d,1) + '0';
        *buf++ = c;
    }
    *buf++ = neg ? '-':'+';
}


// perform the operation (Read, Write) on RAMAC unit file
// init file if len=0 (flat format)
// 
t_stat dsk_operation(int cmd, int unit, int arm, int disk, int track)
{

    FILE *f;
    int flen, i, ic, ZeroNeg;
    char buf[DISK_SIZE+1]; 
    t_int64 d;
    char s[6];
                           // buf holds a full disk

    if ((unit < 0) || (unit > 3)) return 0;
    if ((arm < 0)  || (arm > 2) ) return 0;
    if ((disk < 0) || (disk > 99)) return 0;
    if ((track < 0) || (track > 99)) return 0;

    f = dsk_unit[unit].fileref; // get disk file from unit; 

    flen = sim_fsize(f);
    if (flen == 0) {
        // new file, fill it with blanks
        memset(buf, 32, sizeof(buf)); // fill with space
        for (i=1;i<1000;i++) buf[i*12*6-1]=13; // ad some cr lo allow text editor to vire ramac file
        buf[sizeof(buf)-1]=0;         // add string terminator
        for(i=0;i<100;i++) sim_fwrite(buf, 1, DISK_SIZE, f);
    }
    sim_fseek(f, DISK_SIZE * disk, SEEK_SET);
    sim_fread(buf, 1, DISK_SIZE, f); // read the entire disc (100 tracks)
    ic = 12 * 60 * track;            // ic is char at beginning of track
    sim_debug(DEBUG_DETAIL, &cpu_dev, "... RAMAC file at fseek %d, ic %d\n", DISK_SIZE * disk, ic);
    if (cmd==OP_RDS) {
        for(i=0;i<60;i++) {
            dsk_read_numeric_word(&buf[ic], &d, &ZeroNeg);
            ic += 12; // 12 bytes per word
            // store into IAS
            IAS[i] = d;
            IAS_NegativeZeroFlag[i] = ZeroNeg;
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... RAMAC to IAS %04d: %06d%04d%c '%s'\n", 
                        i+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
        }
        // set IAS_TimingRing. Nothing said in RAMAC manual, but needed to make supersoap CDD pseudo op work properly
        IAS_TimingRing=0;
    } else if (cmd==OP_WDS) {
        for(i=0;i<60;i++) {
            // read IAS
            d = IAS[i];
            ZeroNeg = IAS_NegativeZeroFlag[i];
            sim_debug(DEBUG_DETAIL, &cpu_dev, "... IAS %04d to RAMAC: %06d%04d%c '%s'\n", 
                        i+9000, printfw(d,ZeroNeg), 
                        word_to_ascii(s, 1, 5, d));
            // write numeric to disk buf
            dsk_write_numeric_word(&buf[ic], d, ZeroNeg);
            ic += 12;
        }
        // set IAS_TimingRing. Nothing said in RAMAC manual, but needed to make supersoap CDD pseudo op work properly
        IAS_TimingRing=0;
        // write back disk to ramac unit file
        sim_fseek(f, DISK_SIZE * disk, SEEK_SET);
        sim_fwrite(buf, 1, DISK_SIZE, f); // write the entire disc (100 tracks)
    }
    // don't know if Seek Opcode (SDS) also sets TimingRing to zero

    return SCPE_OK;
}


// return 1 if disk unit n (0..3) and arm (0..2) is ready to receive a command
//        0                                      is busy executing some command
//       -1                                      cannot receive command

int dsk_ready(int unit, int arm)
{
    if ((unit < 0) || (unit > 3)) return -1; // invalid unit
    if ((arm < 0)  || (arm > 2) ) return -1; // invaid arm
    if (dsk_unit[unit].flags & UNIT_DIS) return -1; // disabled
    if ((dsk_unit[unit].flags & UNIT_ATT) == 0) return -1; // not attached
    if (Arm[unit][arm].cmd == 0) return 1; // arm has no cmd to execute -> it is ready to receive new command
    return 0; // arm busy
}

int nseq; 
void add_seq(int unit, int arm, int msec, int hint, int disk, int track)
{
    if (nseq>=Arm_anim_sequence_len) {
        fprintf(stderr, "Cannot add Arm sequence step. Animation array full\n");
         Arm[unit][arm].seq[Arm_anim_sequence_len-1].msec = 0; // mark last as end of sequence
        return;
    }

    Arm[unit][arm].seq[nseq].disk = disk;
    Arm[unit][arm].seq[nseq].track = track;
    Arm[unit][arm].seq[nseq].msec = msec; 
    Arm[unit][arm].seq[nseq].hint = hint; 
    nseq++;
}

// return total time in msec it will take the complete movement sequence
int dsk_set_mov_seq(int unit,int arm)
{
    // set arm movement sequence to its destination
    //
    // arm timing
    //    seek: 50 msec setup time 
    //          on same disk:
    //              1 msec per track in same disk (0-99)
    //              25 msec  sensing track gap (that identifies the start of track pos) a mean between 0-50 msec or
    //                       to extract arm outside disk for arm to go to another disk
    //          going to another physical disk:
    //               200 msec start arm vertical motion
    //                6  msec per physical disk (0 to 49)
    //               200 msec stop arm vertical motion
    //               
    //   read: 110 msec
    //   write: 135 msec
    //

    int cmd, i, d1, d2, dy, di, tr, msec, time;
    static int ArmVertAccel[] = {40,25,15,10};
    static int ArmVertDecel[] = {30,20,10};

    cmd = Arm[unit][arm].cmd;
    nseq = 0;

    // seek or read/write but current arm pos not the addr selected for 
    // read/write -> must do a seek cycle 
    if ((cmd == OP_SDS) || 
        (Arm[unit][arm].current_disk  != Arm[unit][arm].dest_disk) ||
        (Arm[unit][arm].current_track != Arm[unit][arm].dest_track)) {
        // start seek sequence at current arm pos
        msec=50; // msec needed for seek setup time
        add_seq(unit, arm, msec, 0, 
                Arm[unit][arm].current_disk, Arm[unit][arm].current_track);
           // is arm already accessing physical destination disk?
        if ((d1=(Arm[unit][arm].current_disk % 50)) != (d2=(Arm[unit][arm].dest_disk % 50))) {
            // not yet, should move arm up or down
            // is arm outside physical disk stack?
            if (Arm[unit][arm].current_track >= 0) {
                // not yet, should move arm outside physical disk (up to -1)
                // move out arm track to track until outside of physical disk
                for (i=Arm[unit][arm].current_track;i>=0;i--) {
                    msec = 1; // msec needed for horizontal arm movement of 1 track                    
                    add_seq(unit, arm, msec, 0,
                            Arm[unit][arm].current_disk, i);
                }
            }
            // now arm is outside disk stack, can move up and down
            msec=100; // msec needed to setup vertical arm movement 
            add_seq(unit, arm, msec, 0,
                    Arm[unit][arm].current_disk, -1);
            // move out up/down on disk stack up to destination disk
            dy = (d1 < d2) ? +1:-1;
            di = Arm[unit][arm].current_disk;
            for (;;) {
                if (di % 50 == d2) break;
                msec = 6; // msec needed for vertical arm movement of 1 physical disk
                add_seq(unit, arm, msec, (dy < 0) ? 1:2, // hint=1 -> arm going up. =2 -> arm going down
                        di, -1);
                di=di+dy;
            }
            // stop motion and select destination disk (not physical disk)
            msec=150; // msec needed to stop vertical arm movement 
            add_seq(unit, arm, msec, 0, 
                    Arm[unit][arm].dest_disk, -1);
        }
        // now arm accessing physical destination disk
        tr = Arm[unit][arm].seq[nseq-1].track; // current track arm is positioned on
        // is arm at destination track?
        if (tr != (d2=Arm[unit][arm].dest_track)) {
            // not yet, should move arm horizontally
            dy = (tr < d2) ? +1:-1;
            for (;;) {
                if (tr == d2) break;
                msec = 2; // msec needed for horizontal arm movement of 1 track                    
                add_seq(unit, arm, msec, (dy < 0) ? 3:4, // hint=3 -> arm going outside. =4 -> arm going inside disk
                        Arm[unit][arm].dest_disk, tr);
                tr=tr+dy;
            }
        }
        // now arm is positioned on destination track, disk
        // sense the track gap to finish seek operation
        msec = 25; // msec needed for sensing track gap
        add_seq(unit, arm, msec, 0, 
                Arm[unit][arm].dest_disk, Arm[unit][arm].dest_track);
    }

    // read operation
    msec=0;
    if (cmd == OP_RDS) {
        msec=110; // msec needed for reading entire track
    } else if (cmd == OP_WDS) {
        msec=135; // msec needed for writing entire track
    }
    if (msec>0) {
        // if read/write, divide time by 6 to transfer data to/form IAS 
        for (i=1; i<=6;i++) { 
            add_seq(unit, arm, msec/6, 10+i, // Hint = 11..16 transfer to/from IAS
                    Arm[unit][arm].dest_disk, Arm[unit][arm].dest_track);
        }
    }
    // set end of sequence
    add_seq(unit, arm, 0, 0, 
            Arm[unit][arm].dest_disk, Arm[unit][arm].dest_track);
    // calculate the total time in msec of the sequence
    i=0; time=0;
    for(;;) {
        msec=Arm[unit][arm].seq[i].msec;
        if (msec==0) break; // exit because end of sequence
        time=time+msec;
        i++; 
    }
    return time; 
}

// Start off a RAMAC command 
// return in total_msec the total time in msec will be needed to exec the disk command
t_stat dsk_cmd(int cmd, int addr, int * total_msec, int fast)
{
    DEVICE             *dptr;
    UNIT               *uptr;
    int                 ad,unit, disk, track, arm; 
    int                 time; 
    int                 bFastMode; 
    t_stat              r;

    ad = abs(addr); 
    arm   = ad % 10;  ad=ad / 10; 
    track = ad % 100; ad=ad / 100; 
    disk  = ad % 100; ad=ad / 100; 
    unit  = ad % 10;  

    *total_msec=0;

    time = 0;
    /* Make sure addr unit number */
    if ((unit > 3) || (unit < 0)) return STOP_ADDR;
    if ((arm  > 2) || (arm  < 0)) return STOP_ADDR;

    uptr = &dsk_unit[unit];
    dptr = find_dev_from_unit(uptr);
    
    // init IBM 652 Control Unit internal registers
    bFastMode = fast;

    /* If disk unit disabled return error */
    if (uptr->flags & UNIT_DIS) {
        sim_debug(DEBUG_EXP, dptr, "RAMAC command attempted on disabled unit %d\n", unit);
        // not stated in manual: what happends if command to non existant disk?
        // what the simulator do: stops the cpu, lit the ramac checking light
        StopIOError = 4; // lit ramac checking light
        return STOP_IO;
    }
    /* If disk unit has no file attached return error */
    if ((uptr->flags & UNIT_ATT) == 0) {
        sim_debug(DEBUG_EXP, dptr, "RAMAC command attempted on unit %d that has no file attached\n", unit);
        StopIOError = 4; // lit ramac checking light
        return STOP_IO;
    }
    if ((cmd != OP_SDS) && (cmd != OP_RDS) && (cmd != OP_WDS)) {
        sim_debug(DEBUG_EXP, dptr, "RAMAC unkonwn command %d attempted on unit %d\n", cmd, unit);
        StopIOError = 4; // lit ramac checking light
        return STOP_IO;
    }
    sim_debug(DEBUG_CMD, dptr, "RAMAC unit %d, arm %d: %s on disk %d, track %d started\n", 
                               unit, arm, 
                               (cmd == OP_SDS) ? "SEEK" : (cmd == OP_RDS) ? "READ" : "WRITE",
                               disk, track);
    r = dsk_operation(cmd, unit, arm, disk, track);
    if (r != SCPE_OK) {
        StopIOError = 4; // lit ramac checking light
        return STOP_IO;
    }
    if (bFastMode) {
        // fast mode -> set disk comand executed and return
        Arm[unit][arm].current_disk  = Arm[unit][arm].dest_disk  = disk;   // the destination address
        Arm[unit][arm].current_track = Arm[unit][arm].dest_track = track;
        Arm[unit][arm].InitWordTime = -1; // no movement sequence. Just go to destination pos inmediatelly and exec command
        // set arm as ready, so it can accept new commands
        Arm[unit][arm].cmd = 0;
        LastDiskCmd = 0;
        sim_debug(DEBUG_DETAIL, &cpu_dev, "RAMAC unit %d, arm %d: %s on disk %d, track %d finished (fast mode)\n", 
                                           unit, arm, 
                                           (cmd == OP_SDS) ? "SEEK" : (cmd == OP_RDS) ? "READ" : "WRITE",
                                           disk, track);
        // cmd execution finished, fast mode does not set IAS interlock 
        return SCPE_OK;
    }

    // init arm operation
    Arm[unit][arm].cmd = cmd;           // the command to execute: can be OP_SDS, OP_RDS, OP_WDS
    Arm[unit][arm].dest_disk  = disk;   // the destination address
    Arm[unit][arm].dest_track = track;
    Arm[unit][arm].InitWordTime = GlobalWordTimeCount; // when the movement sequence starts (in word time counts)
    // calculate the movement sequence. return number of msec it will take to complete
    *total_msec=dsk_set_mov_seq(unit,arm);
    sim_debug(DEBUG_DETAIL, dptr, "RAMAC unit %d, arm %d: now at disk %d, track %d\n", 
                               unit, arm, Arm[unit][arm].current_disk, Arm[unit][arm].current_track);
    sim_debug(DEBUG_DETAIL, dptr, "                     must %s on disk %d, track %d \n", 
                               (cmd == OP_SDS) ? "SEEK" : (cmd == OP_RDS) ? "READ" : "WRITE",
                               disk, track);
    sim_debug(DEBUG_DETAIL, dptr, "                     will take %d msec, %d WordTimes\n", 
                               *total_msec, msec_to_wordtime(*total_msec));
    // schedule command execution
    time = msec_to_wordtime(UPDATE_RAMAC); // sampling disk arm movement sequence each 10 msec converted to wordcount
    sim_cancel(uptr);
    sim_activate(uptr, time);
    return SCPE_OK_INPROGRESS;
}

/* Handle processing of disk requests. */
t_stat dsk_srv(UNIT * uptr)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    int                 time, msec, arm, cmd, nseq, hint;
    t_int64             InitTime, nWordTimes; 
    int                 bSequenceInProgress=0; 
    int                 bFastMode, n; 

    // init IBM 652 Control Unit internal registers
    bFastMode = 0;
    // update arm movement for this unit
    for (arm=0;arm<3;arm++) {
        cmd = Arm[unit][arm].cmd;
        if (cmd == 0) continue; // RAMAC arm for this disk unit is stoped (=ready). 
                                // continue to Process next arm of this unit
        // arm in movement (=busy)
        // calc time in msec elapsed from start of comand execution
        InitTime=Arm[unit][arm].InitWordTime;
        if (InitTime<0) {
            bFastMode=1;
        } else {
            // calc how many wordtimes elapsed, the convert this to msec
            nWordTimes = GlobalWordTimeCount - Arm[unit][arm].InitWordTime;
            if ((CpuSpeed_Acceleration > 0) && (CpuSpeed_Acceleration < 100)) {
                // if cpu speed slower than real harware, scale  
                time=(int)(nWordTimes * 0.096 * 100/CpuSpeed_Acceleration); 
            } else {
                // convert wordtimes to milisec taking into account the cpuacceleration
                time=(int)(nWordTimes * 0.096); 
            }
            // examine sequence of arm movements to determine what is the current position 
            // of arm at this point of time
            nseq=0;
            for(;;) {
                msec=Arm[unit][arm].seq[nseq].msec;
                if (msec==0) break; // exit beacuse end of sequence
                time=time-msec;
                if (time<0) break; // exit beacuse we are at this point of sequence
                nseq++; 
            }
            if (time <0) {
                // sequence not finished: set current arm pos 
                Arm[unit][arm].current_disk=Arm[unit][arm].seq[nseq].disk;
                Arm[unit][arm].current_track=Arm[unit][arm].seq[nseq].track;
                // signal if accesing IAS
                LastDiskCmd = (LastDiskCmd % 100);  
                hint=Arm[unit][arm].seq[nseq].hint;
                if ((hint>10) && (hint < 20 )) {
                    LastDiskCmd = LastDiskCmd + (hint % 10) * 100;
                }
                // there is an arm in movement
                bSequenceInProgress=1; 
                // arm not arrived to its destination yet. contiinue proceed with next arm
                sim_debug(DEBUG_CMD, dptr, "... RAMAC unit %d, arm %d: now at disk %d, track %d\n", 
                                unit, arm, 
                                Arm[unit][arm].current_disk, Arm[unit][arm].current_track);
                continue; 
            }
        }
        // arm finished the movement sequence and arrived to its destination position
        Arm[unit][arm].current_disk=Arm[unit][arm].dest_disk;
        Arm[unit][arm].current_track=Arm[unit][arm].dest_track;
        // remove ramac access to ias if any
        LastDiskCmd = (LastDiskCmd % 100);  
        // cmd execution finished
        sim_debug(DEBUG_DETAIL, &cpu_dev, "... RAMAC unit %d, arm %d: %s on disk %d, track %d finished\n", 
                                    unit, arm, 
                                    (cmd == OP_SDS) ? "SEEK" : (cmd == OP_RDS) ? "READ" : "WRITE",
                                    Arm[unit][arm].dest_disk, Arm[unit][arm].dest_track);
        // set arm as ready, so it can accept new commands
        Arm[unit][arm].cmd = 0;
        sim_debug(DEBUG_CMD, dptr, "RAMAC unit %d, arm %d READY\n", unit, arm);
    }
    // if there is any arm in movement, re-schedulle event 
    sim_cancel(uptr);
    if (bSequenceInProgress) {
        time = msec_to_wordtime(UPDATE_RAMAC); // sampling disk arm movement sequence each 10 msec
        sim_activate(uptr, time);
    }
    return SCPE_OK;
}

void dsk_ini(UNIT * uptr, t_bool f)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);

    memset(&Arm[unit], 0, sizeof(Arm[unit])); // zeroes arm info for this unit
} 

t_stat dsk_reset(DEVICE * dptr)
{
    int i;
    for (i = 0; i < 4; i++) {
        dsk_ini(&dsk_unit[i], 0);
    }
    return SCPE_OK;
}

t_stat dsk_attach(UNIT * uptr, CONST char *file)
{
    DEVICE             *dptr = find_dev_from_unit(uptr);
    int                 unit = (uptr - dptr->units);
    t_stat              r;
    int                    flen;

    if ((r = attach_unit(uptr, file)) != SCPE_OK) return r;
    flen=sim_fsize(uptr->fileref);
    if ((flen > 0) && (flen != DISK_SIZE * 100)) {
        sim_messagef (SCPE_IERR, "Invalid RAMAC Unit file size\n");
        detach_unit (uptr); 
    }
    dsk_ini(uptr, 0);
    return SCPE_OK;
}

t_stat dsk_detach(UNIT * uptr)
{
    sim_cancel(uptr); // cancel any pending command
    dsk_ini(uptr, 0);
    return detach_unit (uptr);                             /* detach unit */
}

t_stat dsk_help(FILE *st, DEVICE *dptr, UNIT *uptr, int32 flag, const char *cptr)
{
   fprintf (st, "%s\n\n", dsk_description(dptr));
   fprintf (st, "RAMAC Magnetic Disk Storage Unit.\n\n");
   fprint_set_help(st, dptr);
   fprint_show_help(st, dptr);
   return SCPE_OK;
}

const char * dsk_description(DEVICE *dptr)
{
   return "IBM 355 RAMAC Disk Storage Unit";
}


