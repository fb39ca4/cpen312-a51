/*  emitter.c

    Copyright (C) 2010, 2012  Jesus Calvino-Fraga, jesusc (at) ece.ubc.ca
    
    Derived from as31 - 8031/8051 Assembler by Ken Stauffer
    
	This program is free software; you can redistribute it and/or modify it
	under the terms of the GNU General Public License as published by the
	Free Software Foundation; either version 2, or (at your option) any
	later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "a51.h"

/* Prototypes for open(), close(), addr(), and byte() routines for different formats. */

static int open_hex(const char *file, const char *ftype, const char *arg);
static void close_hex(void);
static void addr_hex(unsigned long a);
static void byte_hex(unsigned char b);

static int open_bin(const char *file, const char *ftype, const char *arg);
static void close_bin(void);
static void addr_bin(unsigned long a);
static void byte_bin(unsigned char b);

/* ----------------------------------------------------------------------
 * ADD an entry to this table to register your
 * output format routines. Give your object format
 * a name to be specified with the -F option.
 *
 */

static int format;

static struct
{
    char *name;
    char *extension;
    char *description;
    int (*e_open)(const char *file, const char *ftype, const char *arg);
    void (*e_close)(void);
    void (*e_addr)(unsigned long a);
    void (*e_byte)(unsigned char b);
} formtab[] =
{
    { "hex",   "hex",  "Intel-Hex (.hex)",  open_hex,  close_hex,  addr_hex,  byte_hex },
    { "bin",   "bin",  "Binary (.bin)",  open_bin,  close_bin,  addr_bin,  byte_bin }
};

#define FORMTABSIZE (sizeof(formtab)/sizeof(formtab[0]))

void emitusage(void)
{
    int i;

    fprintf(stderr, "\tfmt is one of:");
    for(i=0; i<FORMTABSIZE; )
	{
        fprintf(stderr, "%s", formtab[i].name);
        if( ++i < FORMTABSIZE)
            fprintf(stderr, ", ");
    }
    fprintf(stderr, ".\n");
}

const char *emit_extension(const char *ftype)
{
    int i;

    if (ftype)
	{
        for(i=0; i<FORMTABSIZE; i++ )
		{
            if (!strcmp(formtab[i].name, ftype))
                return formtab[i].extension;
        }
    }
    return formtab[0].extension;
}


const char *emit_desc_lookup(int num)
{
    if (num >= 0 && num < FORMTABSIZE)
        return formtab[num].description;
    return NULL;
}

const char *emit_desc_to_name_lookup(const char *desc)
{
    int i;

    if (desc)
	{
        for(i=0; i<FORMTABSIZE; i++ )
		{
            if (!strcmp(formtab[i].description, desc))
                return formtab[i].name;
        }
    }
    return NULL;
}


int emitopen(const char *file, const char *ftype, const char *arg)
{
    int i;
    if( ftype )
	{
        for(i=0; i<FORMTABSIZE; i++ )
		{
            if( !strcmp(formtab[i].name,ftype) )
			{
                format = i;
                return (*formtab[format].e_open)(file,ftype,arg);
            }
        }
        mesg_f("no format \"%s\", using \"%s\"\n", ftype, formtab[0].name);
    }
    /*
     * 0th entry is the default format type
     */
    format = 0;
    return (*formtab[format].e_open)(file, ftype, arg);
}

void emitclose(void)
{
    (*formtab[format].e_close)();
}

void emitaddr(unsigned long a)
{
    (*formtab[format].e_addr)(a);
}

void emitbyte(int b)
{
    (*formtab[format].e_byte)((unsigned char)b);
}

/* ----------------------------------------------------------------------
 * Individual file format routines appear here:
 *  Each file format must define the following routines:
 *      open()  - Called ONCE before any of the others.
 *          It is passed with a filename and a format
 *          specific argument.
 *
 *      close() - Called ONCE when no more emit_byte()
 *          function calls will be made.
 *
 *      addr() - Called when ever a new address has been set
 *          in the assembler (ie. .org, .skip).
 *          This routine is also called once when the
 *          location counter is set to 0 at the very start of
 *          assembling.
 * 
 *      byte() - Called with each byte to be outputed.
 *
 */

static unsigned long addr;
static FILE *fout=NULL;
static long int offset;
static int newaddr;
static int pos=0;
static unsigned char bytes[16];

extern int ObjectFile; // For object files, the assembler doesn't create the hex/bin file.

/* Intel HEX format */
void hexdump(void)     /* dumps one line into file */
{
    int i, sum;

    if (fout == NULL) return;

    fprintf(fout,":%02X%04lX00", pos, addr & 0xFFFF);

    sum = pos + ((addr>>8)&0xff) + (addr&0xff) ;

    for (i=0; i < pos; i++)
	{
        fprintf(fout,"%02X", bytes[i] & 0xFF );
        sum += bytes[i]&0xff;
    }
    fprintf(fout, "%02X\n", (-sum)&0xff);
    addr += pos;
    pos = 0;
}

static int open_hex(const char *file, const char *ftype, const char *arg)
{
	if(ObjectFile==1)
	{
		fout = NULL;
		return 0;
	}
    if (file == NULL) fout = stdout;
    else fout = fopen(file, "w");

    if( fout == NULL )
	{
        mesg_f("Cannot open %s for writing.\n", file);
        return -1;
    }
    pos = 0;
    return 0;
}

static void close_hex(void)
{
    if (fout == NULL) return;
    if ( pos > 0 ) hexdump();
    fprintf(fout, ":00000001FF\n");  /* end of file record */
    fclose(fout);
}

static void addr_hex(unsigned long a)
{
    if ( pos > 0 ) hexdump();
    addr = a;
}

static void byte_hex(unsigned char b)
{
    bytes[pos] = b;
    pos += 1;
    if ( pos == 16) hexdump();
}

/* "bin" format.  Raw binary data */

static int open_bin(const char *file, const char *ftype, const char *arg)
{
	if(ObjectFile==1)
	{
		fout = NULL;
		return 0;
	}
    if (file == NULL) fout = stdout;
    else fout = fopen(file, "w");

    if( fout == NULL ) {
        mesg_f("Cannot open %s for writing.\n", file);
        return -1;
    }
    addr = 0;
    return 0;
}

static void close_bin(void)
{
    if (fout == NULL) return;
    fclose(fout);
}

static void addr_bin(unsigned long a)
{
    unsigned long i;

    if (fout == NULL) return;
    if (a > addr)
	{
        for (i=0; i < a - addr; i++)
		{
            fprintf(fout, "%c", 0);
        }
        addr = a;
        return;
    }
    if (a < addr)
	{
        fatalerror("address changed to %lX, from %lX", a, addr);
        fatalerror("binary output format can't write backwards");
        addr = a;
        return;
    }
}

static void byte_bin(unsigned char b)
{
    if (fout == NULL) return;
    fprintf(fout, "%c", b & 0xFF);
    addr++;
}

