/*  mp-b2.c: SWTP SS-50/SS-30 MP-B2 Mother Board

    Copyright (c) 2011-2012, William A. Beech

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
        WILLIAM A. BEECH BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
        IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
        CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

        Except as contained in this notice, the name of William A. Beech shall not be
        used in advertising or otherwise to promote the sale, use or other dealings
        in this Software without prior written authorization from William A. Beech.

    MODIFICATIONS:

        24 Apr 15 -- Modified to use simh_debug
        Jun/2022 -- Modified to support additional devices

    NOTES:

*/

#include <stdio.h>
#include "swtp_defs.h"

#define UNIT_V_RAM_0000   (UNIT_V_UF)   /* MP-8M board 0 enable */
#define UNIT_RAM_0000     (1 << UNIT_V_RAM_0000)
#define UNIT_V_RAM_2000   (UNIT_V_UF+1) /* MP-8M board 1 enable */
#define UNIT_RAM_2000     (1 << UNIT_V_RAM_2000)
#define UNIT_V_RAM_4000   (UNIT_V_UF+2) /* MP-8M board 2 enable */
#define UNIT_RAM_4000     (1 << UNIT_V_RAM_4000)
#define UNIT_V_RAM_6000   (UNIT_V_UF+3) /* MP-8M board 3 enable */
#define UNIT_RAM_6000     (1 << UNIT_V_RAM_6000)
#define UNIT_V_RAM_A000   (UNIT_V_UF+4) /* MP-8M board 4 enable */
#define UNIT_RAM_A000     (1 << UNIT_V_RAM_A000)
#define UNIT_V_RAM_C000   (UNIT_V_UF+5) /* MP-8M board 5 enable */
#define UNIT_RAM_C000     (1 << UNIT_V_RAM_C000)

t_stat set_64k (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat set_64k_iobase (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat set_64k_rombase (UNIT *uptr, int32 value, CONST char *cptr, void *desc);
t_stat show_64k(FILE *st, UNIT *uptr, int32 val, CONST void *desc);

/* function prototypes */

int32 get_base(void);
int32 CPU_BD_get_mbyte(int32 addr);
int32 CPU_BD_get_mword(int32 addr);
void CPU_BD_put_mbyte(int32 addr, int32 val);
void CPU_BD_put_mword(int32 addr, int32 val);

/* empty I/O device routine */
int32 nulldev(int32 io, int32 data);

/* SS-50 bus routines */
int32 MB_get_mbyte(int32 addr);
int32 MB_get_mword(int32 addr);
void MB_put_mbyte(int32 addr, int32 val);
void MB_put_mword(int32 addr, int32 val);
t_stat mpb2_examine(t_value *eval_array, t_addr addr, UNIT *uptr, int32 switches);
t_stat mpb2_deposit(t_value value, t_addr addr, UNIT *uptr, int32 switches);

/* MP-8M bus routines */
extern int32 mp_8m_get_mbyte(int32 addr);
extern void mp_8m_put_mbyte(int32 addr, int32 val);

/* SS-50 I/O address space functions */

/* MP-S serial I/O routines */
extern int32 sio0s(int32 io, int32 data);
extern int32 sio0d(int32 io, int32 data);
extern int32 sio1s(int32 io, int32 data);
extern int32 sio1d(int32 io, int32 data);

/* DC-4 FDC I/O routines */
extern int32 dc4_fdcdrv(int32 io, int32 data);
extern int32 dc4_fdccmd(int32 io, int32 data);
extern int32 dc4_fdctrk(int32 io, int32 data);
extern int32 dc4_fdcsec(int32 io, int32 data);
extern int32 dc4_fdcdata(int32 io, int32 data);

/* LFD-400 FDC I/O routines */
extern int32 fd400_fdcstatus(int32 io, int32 data);
extern int32 fd400_cstatus(int32 io, int32 data);
extern int32 fd400_data(int32 io, int32 data);
extern int32 fd400_cursect(int32 io, int32 data);
extern int32 fd400_startrw(int32 io, int32 data);
extern UNIT  fd400_dsk_unit[]; 

/* MP-T Timer I/O routines */
extern int32 timer0pia(int32 io, int32 data);
extern int32 timer1pia(int32 io, int32 data);

/* MP-LA Parallel Card for Line Printer I/O routines */
extern int32 par0pia(int32 io, int32 data);  // par0pia and par1pia simulates SWTPC PR-40 line printer
extern int32 par1pia(int32 io, int32 data);
extern int32 par26pia(int32 io, int32 data); // par26pia and par27pia simulates exorciser line printer
extern int32 par27pia(int32 io, int32 data);
extern int32 lpt_iobase;                     
extern int32 lpt_type;                       // 0=disabled, 1=pr40 printer, 2=exorciser printer (M68SP702 702 Printer with MEX68PI Printer Interface I/O Module)

// Motorola Exorciser Line Printer
extern int32 par26pia(int32 io, int32 data);
extern int32 par27pia(int32 io, int32 data);

/* HLE-HD I/O routines */
extern int32 hd0cmd(int32 io, int32 data);
extern int32 hd1data(int32 io, int32 data);

/* iCOM FD360 FDC I/O routines */
extern int32 fd360_dkdid(int32 io, int32 data);
extern int32 fd360_dkdic(int32 io, int32 data);
extern int32 fd360_dkcod(int32 io, int32 data);
extern int32 fd360_dkcoc(int32 io, int32 data);
extern int32 fd360_dkdod(int32 io, int32 data);
extern int32 fd360_dkdoc(int32 io, int32 data);
extern int32 fd360_iobase; // default addr for disk controller PIAs
extern DEVICE fd360_dsk_dev; 

/* Optional serial port I/O routines */
extern int32 sio0s_port(int32 io, int32 data);
extern int32 sio0d_port(int32 io, int32 data);
extern int32 sio1s_port(int32 io, int32 data);
extern int32 sio1d_port(int32 io, int32 data);
extern int32 sio_port_iobase; // default addr for ACIAs
extern int ac30_mode;         // cassette 0=off, <>0 on
extern int sio_port_mode;     // 0=off, 1=on


/* This is the I/O configuration table.  There are 32 possible
device addresses, if a device is plugged into a port it's routine
address is here, 'nulldev' means no device is available
*/

struct idev dev_table[32] = {
        {&sio0s_port},  {&sio0d_port},  {&sio1s_port},  {&sio1d_port},  /* Port 0 8000-8003 */
        {&sio0s},       {&sio0d},       {&sio1s},       {&sio1d},       /* Port 1 8004-8007 */
/* sio1x routines just return the last value read on the matching
   sio0x routine.  SWTBUG tests for the MP-C with most port reads! */
        {&nulldev},     {&nulldev},     {&nulldev},     {&nulldev},     /* Port 2 8008-800B */
        {&nulldev},     {&nulldev},     {&nulldev},     {&nulldev},     /* Port 3 800C-800F */
        // addr 800C and 800D are used by gt6144 graphic card
        //      800E and 800F             ppg-j analog joystick
        {&hd0cmd},     {&hd1data},     {&timer0pia},   {&timer1pia},    /* Port 4 8010-8013 */
        // addr 8012 and 8013 are used by mp-t timer card
        // addr 8010 and 8011 are used by HLE-Hard Disk 
        {&dc4_fdcdrv},  {&nulldev},     {&nulldev},     {&nulldev},     /* Port 5 8014-8017 */
        {&dc4_fdccmd},  {&dc4_fdctrk},  {&dc4_fdcsec},  {&dc4_fdcdata}, /* Port 6 8018-801B */
        // addr 8018 and 8019 are also used by Graph1 terminal
        {&par0pia},     {&par1pia},     {&nulldev},     {&nulldev}      /* Port 7 801C-801F */
        // addr 801C is used by MP-LA Paraller interface Board for line printer
};

struct idev dev_table2[8] = {
/* LFD-400 routines */
        {&fd400_cstatus} /* addr CC00 */, {&fd400_data}      /* addr CC01 */,
        {&fd400_cursect} /* addr CC02 */, {&fd400_fdcstatus} /* addr CC03 */, 
        {&fd400_startrw} /* addr CC04 */, {&nulldev},     
        {&nulldev},                       {&nulldev}
};

struct idev dev_table3[8] = {
/* iCOM FD360 routines */
        {&fd360_dkdid} /* addr F800 */, {&fd360_dkdic}       /* addr F801 */,
        {&fd360_dkcod} /* addr F802 */, {&fd360_dkcoc}       /* addr F803 */, 
        {&nulldev},                     {&nulldev},
        {&fd360_dkdod} /* addr F806 */, {&fd360_dkdoc}       /* addr F807 */ 
};


/* dummy i/o device */

int32 nulldev(int32 io, int32 data)
{
    if (io == 0)
        return (0xFF);
    return 0;
}

/* Mother Board data structures

    MB_dev        Mother Board device descriptor
    MB_unit       Mother Board unit descriptor
    MB_reg        Mother Board register list
    MB_mod        Mother Board modifiers list
*/

UNIT MB_unit = { 
    UDATA (NULL, 0, 0)
};

REG MB_reg[] = {
    { NULL }
};

MTAB MB_mod[] = {
    { UNIT_RAM_0000, UNIT_RAM_0000, "BD0 On", "BD0", NULL },
    { UNIT_RAM_0000, 0, "BD0 Off", "NOBD0", NULL },
    { UNIT_RAM_2000, UNIT_RAM_2000, "BD1 On", "BD1", NULL },
    { UNIT_RAM_2000, 0, "BD1 Off", "NOBD1", NULL },
    { UNIT_RAM_4000, UNIT_RAM_4000, "BD2 On", "BD2", NULL },
    { UNIT_RAM_4000, 0, "BD2 Off", "NOBD2", NULL },
    { UNIT_RAM_6000, UNIT_RAM_6000, "BD3 On", "BD3", NULL },
    { UNIT_RAM_6000, 0, "BD3 Off", "NOBD3", NULL },
    { UNIT_RAM_A000, UNIT_RAM_A000, "BD4 On", "BD4", NULL },
    { UNIT_RAM_A000, 0, "BD4 Off", "NOBD4", NULL },
    { UNIT_RAM_C000, UNIT_RAM_C000, "BD5 On", "BD5", NULL },
    { UNIT_RAM_C000, 0, "BD5 Off", "NOBD5", NULL },
    { MTAB_XTD | MTAB_VDV, 0,       "NO64K",  "NO64K", &set_64k, &show_64k, NULL},
    { MTAB_XTD | MTAB_VDV, 1,       NULL,     "64K"  , &set_64k, NULL, NULL},
    { MTAB_XTD | MTAB_VDV, 1,       NULL,     "IOBASE"  , &set_64k_iobase, NULL, NULL},
    { MTAB_XTD | MTAB_VDV, 1,       NULL,     "ROMBASE"  , &set_64k_rombase, NULL, NULL},
    { 0 }
};

DEBTAB MB_debug[] = {
    { "ALL", DEBUG_all, "All debug bits" },
    { "FLOW", DEBUG_flow, "Flow control" },
    { "READ", DEBUG_read, "Read Command" },
    { "WRITE", DEBUG_write, "Write Command"},
    { NULL }
};

DEVICE MB_dev = {
    "MP-B2",                            //name
    &MB_unit,                           //units
    MB_reg,                             //registers
    MB_mod,                             //modifiers
    1,                                  //numunits
    16,                                 //aradix
    16,                                 //awidth
    1,                                  //aincr
    16,                                 //dradix
    8,                                  //dwidth
    mpb2_examine,                        //examine
    mpb2_deposit,                        //deposit
    NULL,                               //reset
    NULL,                               //boot
    NULL,                               //attach
    NULL,                               //detach
    NULL,                               //ctxt
    DEV_DEBUG,                          //flags
    0,                                  //dctrl
    MB_debug,                           /* debflags */
    NULL,                               //msize
    NULL                                //lname
};

// 64k RAM Mode:
//	System has ram in 0000..FFFF
//      except 32 bytes at iobase (defaults to FF00..FF1F) that is the I/O 
//      no bootrom, no eprom
//  other devices (fd360, sio-port) can be enables and placed in the memory
//  map at desierd iobase address

int Mode64kRAM=0; 
int Mode64k_iobase=0xFF00; // I/O base address for I/O when 64k RAM active
int Mode64k_rombase=0;     // lowest ROM address to cope with destructive RAM scanning routines (=0 -> not set=

int RAM64k[65536]; 

t_stat show_64k(FILE *st, UNIT *uptr, int32 val, CONST void *desc) 
{
    if (Mode64kRAM == 0) return SCPE_OK;
    fprintf (st, "iobase=%04x", Mode64k_iobase);
    if (Mode64k_rombase) fprintf (st, ", rombase=%04x", Mode64k_rombase);
    return SCPE_OK;
}

t_stat set_64k (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    if (value == 1) {
        // activate 64k RAM
        Mode64kRAM=1;
        memset(RAM64k, 0, sizeof(RAM64k)); 
    } else {
        Mode64kRAM=0;
    }
    return SCPE_OK; 
}

t_stat set_64k_iobase (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    if (Mode64kRAM==0) {
        sim_printf("IOBASE can be set only if 64K mode is active\n");
        return SCPE_ARG;
    }
    num = (int32) get_uint (cptr, 16, 65536, &r); // value is given as hex value
    if (r != SCPE_OK) return r;
    Mode64k_iobase=num; 
    return SCPE_OK; 
}

t_stat set_64k_rombase (UNIT *uptr, int32 value, CONST char *cptr, void *desc)
{
    int num; 
    t_stat r; 

    if (Mode64kRAM==0) {
        sim_printf("ROMBASE can be set only if 64K mode is active\n");
        return SCPE_ARG;
    }
    num = (int32) get_uint (cptr, 16, 65536, &r); // value is given as hex value
    if (r != SCPE_OK) return r;
    Mode64k_rombase=num; 
    return SCPE_OK; 
}

/*  get a byte from memory */

int32 MB_get_mbyte(int32 addr)
{
    int32 val;

    if (Mode64kRAM) {
        if ((fd360_dsk_dev.flags & DEV_DIS) == 0) {
           if ((addr >= fd360_iobase) && (addr < fd360_iobase + 8)) {
              val = (dev_table3[addr - fd360_iobase].routine(0, 0));
              return val; 
           }
        } 
        if (lpt_type != 0) {
            if ((addr >= lpt_iobase) && (addr < lpt_iobase + 2)) {
                if (lpt_type == 2) {
                    if (addr == lpt_iobase) {val = par26pia(0,0);}   /* read status register */
                    else                    {val = par27pia(0,0);}   /* read data register */
                } else {
                    if (addr == lpt_iobase) {val = par0pia(0,0);}    /* read status register */
                    else                    {val = par1pia(0,0);}    /* read data register */
                }
                return val; 
            }
        }
        if ((sio_port_mode != 0) || (ac30_mode !=0 )) {
            if ((addr >= sio_port_iobase) && (addr < sio_port_iobase + 2)) {
               val = (dev_table[addr - sio_port_iobase].routine(0, 0));
               return val; 
            }
        }
        if ((addr >= Mode64k_iobase) && (addr < Mode64k_iobase + 32)) {
            val = (dev_table[addr - Mode64k_iobase].routine(0, 0)) & 0xFF;
        } else {
            val = RAM64k[addr]; 
        }
        return val; 
    }

    switch(addr & 0xE000) {
        case 0x0000:                    //0000-1FFFh
            if (MB_unit.flags & UNIT_RAM_0000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        case 0x2000:                    //2000-3FFFh
            if (MB_unit.flags & UNIT_RAM_2000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        case 0x4000:                    //4000-5FFFh
            if (MB_unit.flags & UNIT_RAM_4000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        case 0x6000:                    //6000-7FFFh
            if (MB_unit.flags & UNIT_RAM_6000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        case 0x8000:                    //8000-9FFFh (I/O ports)
            if (addr < 0x8020) {
                val = (dev_table[addr - 0x8000].routine(0, 0)) & 0xFF;
            } else
                val = 0xFF;
            sim_debug (DEBUG_read, &MB_dev, "MB_get_mbyte: I/O addr=%04X val=%02X\n",
                addr, val);
            break;
        case 0xA000:                    //A000-BFFFh
            if (MB_unit.flags & UNIT_RAM_A000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        case 0xC000:                    //C000-CFFFh
            if (((fd400_dsk_unit[0].flags & UNIT_DIS) == 0) && ((addr & 0xFFF0) == 0xCC00)) {
                val = (dev_table2[addr - 0xCC00].routine(0, 0));
            } else if (MB_unit.flags & UNIT_RAM_C000)
                val = mp_8m_get_mbyte(addr) & 0xFF;
            else
                val = 0xFF;
            break;
        default:
            val = 0xFF;
    }
    return val;
}

/*  get a word from memory */

int32 MB_get_mword(int32 addr)
{
    int32 val;

    sim_debug (DEBUG_read, &MB_dev, "MB_get_mword: addr=%04X\n", addr);
    val = (MB_get_mbyte(addr) << 8);
    val |= MB_get_mbyte(addr+1);
    val &= 0xFFFF;
    sim_debug (DEBUG_read, &MB_dev, "MB_get_mword: val=%04X\n", val);
    return val;
}

/*  put a byte to memory */

void MB_put_mbyte(int32 addr, int32 val)
{

    if (Mode64kRAM) {
        if ((fd360_dsk_dev.flags & DEV_DIS) == 0) {
            if ((addr >= fd360_iobase) && (addr < fd360_iobase + 8)) {
               dev_table3[addr - fd360_iobase].routine(1, val);
               return; 
            }
        } 
        if (lpt_type != 0) {
            if ((addr >= lpt_iobase) && (addr < lpt_iobase + 2)) {
                if (lpt_type == 2) {
                    if (addr == lpt_iobase) {par26pia(1,val);}   /* read status register */
                    else                    {par27pia(1,val);}   /* read data register */
                } else {
                    if (addr == lpt_iobase) {par0pia(1,val);}    /* read status register */
                    else                    {par1pia(1,val);}    /* read data register */
                }
                return; 
            }
        }
        if ((sio_port_mode != 0) || (ac30_mode !=0 )) {
            if ((addr >= sio_port_iobase) && (addr < sio_port_iobase + 2)) {
               dev_table[addr - sio_port_iobase].routine(1, val);
               return; 
            }
        }
        if ((addr >= Mode64k_iobase) && (addr < Mode64k_iobase + 32)) {
            dev_table[addr - Mode64k_iobase].routine(1, val); // I/0 address FF00 - FF1F
        } else if ((Mode64k_rombase) && ((addr & 0xFF00) == (Mode64k_rombase & 0xFF00))) {
            // the rombase page address is non-writtable/READ-ONLY
        } else {
            RAM64k[addr] = val; 
        }
        return; 
    }

    switch(addr & 0xE000) {
        case 0x0000:                    //0000-1FFFh
            if (MB_unit.flags & UNIT_RAM_0000)
                mp_8m_put_mbyte(addr, val);
            break;
        case 0x2000:                    //2000-3FFFh
            if (MB_unit.flags & UNIT_RAM_2000)
                mp_8m_put_mbyte(addr, val);
            break;
        case 0x4000:                    //4000-5FFFh
            if (MB_unit.flags & UNIT_RAM_4000)
                mp_8m_put_mbyte(addr, val);
            break;
        case 0x6000:                    //6000-7FFFh
            if (MB_unit.flags & UNIT_RAM_6000)
                mp_8m_put_mbyte(addr, val);
            break;
        case 0x8000:                    //8000-9FFFh (I/O ports)
            if (addr < 0x8020)
                dev_table[addr - 0x8000].routine(1, val);
            break;
        case 0xA000:                    //A000-AFFFh
            if (MB_unit.flags & UNIT_RAM_A000)
                mp_8m_put_mbyte(addr, val);
            break;
        case 0xC000:                    //C000-CFFFh
            if (((fd400_dsk_unit[0].flags & UNIT_DIS) == 0) && ((addr & 0xFFF0) == 0xCC00)) {
                dev_table2[addr - 0xCC00].routine(1, val);
            } else if (MB_unit.flags & UNIT_RAM_C000)
                mp_8m_put_mbyte(addr, val);
            break;
        default:
            ;
    }
}

/*  put a word to memory */

void MB_put_mword(int32 addr, int32 val)
{
    sim_debug (DEBUG_write, &MB_dev, "MB_ptt_mword: addr=%04X, val=%04X\n", addr, val);
    MB_put_mbyte(addr, val >> 8);
    MB_put_mbyte(addr+1, val);
}

t_stat mpb2_examine(t_value *eval_array, t_addr addr, UNIT *uptr, int32 switches)
{
    int32 i;

    for (i=0; i<sim_emax; ++i)
        *eval_array++ = CPU_BD_get_mbyte(addr++);
    return SCPE_OK;
}

t_stat mpb2_deposit(t_value value, t_addr addr, UNIT *uptr, int32 switches)
{
    CPU_BD_put_mbyte(addr,value);
    return SCPE_OK;
}

/* end of mp-b2.c */

