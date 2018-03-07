/*  symbol.c

    Copyright (C) 2010, 2011  Jesus Calvino-Fraga, jesusc (at) ece.ubc.ca
    
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

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "a51.h"
#include "parser.h"

extern int CaseSensitive_Flag;

struct symbol * abs_cseg;
struct symbol * abs_dseg;
struct symbol * abs_iseg;
struct symbol * abs_xseg;
struct symbol * abs_bseg;
struct symbol * current_seg;
struct symbol * last_cseg;

#define B(a)        (0xF0+(a))
#define ACC(a)      (0xE0+(a))
#define PSW(a)      (0xD0+(a))
#define T2CON(a)    (0xC8+(a))
#define IP(a)       (0xB8+(a))
#define P3(a)       (0xB0+(a))
#define IE(a)       (0xA8+(a))
#define P2(a)       (0xA0+(a))
#define SCON(a)     (0x98+(a))
#define P1(a)       (0x90+(a))
#define TCON(a)     (0x88+(a))
#define P0(a)       (0x80+(a))

/* ---------------------------------------------------------------------- 
 * sinit[]
 *  These symbols are not reserved keywords.
 *  This array contains the initial symbol table entries
 *  for the user symbol table. The symbols are
 *  basically convinient names that make writing
 *  in 8031/8051 bearable.
 *
 *  The function syminit() inserts these entries into the
 *  symbol table.
 *
 */

static struct symbol sinit[] = {
    { "AC",     NULL, 0, 1, LABEL, PSW(6),   NULL, 1 },
    { "ACC",    NULL, 0, 1, LABEL, ACC(0),   NULL, 1 },
    { "B",      NULL, 0, 1, LABEL, B(0),     NULL, 1 },
    { "CY",     NULL, 0, 1, LABEL, PSW(7),   NULL, 1 },
    { "DPH",    NULL, 0, 1, LABEL, 0x83,     NULL, 1 },
    { "DPL",    NULL, 0, 1, LABEL, 0x82,     NULL, 1 },
    { "EA",     NULL, 0, 1, LABEL, IE(7),    NULL, 1 },
    { "ES",     NULL, 0, 1, LABEL, IE(4),    NULL, 1 },
    { "ET0",    NULL, 0, 1, LABEL, IE(1),    NULL, 1 },
    { "ET1",    NULL, 0, 1, LABEL, IE(3),    NULL, 1 },
    { "ET2",    NULL, 0, 1, LABEL, IE(5),    NULL, 1 },
    { "EX0",    NULL, 0, 1, LABEL, IE(0),    NULL, 1 },
    { "EX1",    NULL, 0, 1, LABEL, IE(2),    NULL, 1 },
    { "EXEN2",  NULL, 0, 1, LABEL, T2CON(3), NULL, 1 },
    { "EXF2",   NULL, 0, 1, LABEL, T2CON(6), NULL, 1 },
    { "F0",     NULL, 0, 1, LABEL, PSW(5),   NULL, 1 },
    { "IE",     NULL, 0, 1, LABEL, IE(0),    NULL, 1 },
    { "IE0",    NULL, 0, 1, LABEL, TCON(1),  NULL, 1 },
    { "IE1",    NULL, 0, 1, LABEL, TCON(3),  NULL, 1 },
    { "INT0",   NULL, 0, 1, LABEL, P3(2),    NULL, 1 },
    { "INT1",   NULL, 0, 1, LABEL, P3(3),    NULL, 1 },
    { "IP",     NULL, 0, 1, LABEL, IP(0),    NULL, 1 },
    { "IT0",    NULL, 0, 1, LABEL, TCON(0),  NULL, 1 },
    { "IT1",    NULL, 0, 1, LABEL, TCON(2),  NULL, 1 },
    { "OV",     NULL, 0, 1, LABEL, PSW(2),   NULL, 1 },
    { "P",      NULL, 0, 1, LABEL, PSW(0),   NULL, 1 },
    { "P0",     NULL, 0, 1, LABEL, P0(0),    NULL, 1 },
    { "P1",     NULL, 0, 1, LABEL, P1(0),    NULL, 1 },
    { "P2",     NULL, 0, 1, LABEL, P2(0),    NULL, 1 },
    { "P3",     NULL, 0, 1, LABEL, P3(0),    NULL, 1 },
    { "PCON",   NULL, 0, 1, LABEL, 0x87,     NULL, 1 },
    { "PS",     NULL, 0, 1, LABEL, IP(4),    NULL, 1 },
    { "PSW",    NULL, 0, 1, LABEL, PSW(0),   NULL, 1 },
    { "PT0",    NULL, 0, 1, LABEL, IP(1),    NULL, 1 },
    { "PT1",    NULL, 0, 1, LABEL, IP(3),    NULL, 1 },
    { "PT2",    NULL, 0, 1, LABEL, IP(5),    NULL, 1 },
    { "PX0",    NULL, 0, 1, LABEL, IP(0),    NULL, 1 },
    { "PX1",    NULL, 0, 1, LABEL, IP(2),    NULL, 1 },
    { "RB8",    NULL, 0, 1, LABEL, SCON(2),  NULL, 1 },
    { "RCAP2H", NULL, 0, 1, LABEL, 0xCB,     NULL, 1 },
    { "RCAP2L", NULL, 0, 1, LABEL, 0xCA,     NULL, 1 },
    { "RCLK",   NULL, 0, 1, LABEL, T2CON(5), NULL, 1 },
    { "RD",     NULL, 0, 1, LABEL, P3(7),    NULL, 1 },
    { "RI",     NULL, 0, 1, LABEL, SCON(0),  NULL, 1 },
    { "RL2",    NULL, 0, 1, LABEL, T2CON(0), NULL, 1 },
    { "RS0",    NULL, 0, 1, LABEL, PSW(3),   NULL, 1 },
    { "RS1",    NULL, 0, 1, LABEL, PSW(4),   NULL, 1 },
    { "RXD",    NULL, 0, 1, LABEL, P3(0),    NULL, 1 },
    { "SBUF",   NULL, 0, 1, LABEL, 0x99,     NULL, 1 },
    { "SCON",   NULL, 0, 1, LABEL, SCON(0),  NULL, 1 },
    { "SM0",    NULL, 0, 1, LABEL, SCON(7),  NULL, 1 },
    { "SM1",    NULL, 0, 1, LABEL, SCON(6),  NULL, 1 },
    { "SM2",    NULL, 0, 1, LABEL, SCON(5),  NULL, 1 },
    { "SP",     NULL, 0, 1, LABEL, 0x81,     NULL, 1 },
    { "T0",     NULL, 0, 1, LABEL, P3(4),    NULL, 1 },
    { "T1",     NULL, 0, 1, LABEL, P3(5),    NULL, 1 },
    { "T2",     NULL, 0, 1, LABEL, P0(0),    NULL, 1 },
    { "T2CON",  NULL, 0, 1, LABEL, T2CON(0), NULL, 1 },
    { "T2EX",   NULL, 0, 1, LABEL, P0(1),    NULL, 1 },
    { "TB8",    NULL, 0, 1, LABEL, SCON(3),  NULL, 1 },
    { "TCLK",   NULL, 0, 1, LABEL, T2CON(4), NULL, 1 },
    { "TCON",   NULL, 0, 1, LABEL, TCON(0),  NULL, 1 },
    { "TF0",    NULL, 0, 1, LABEL, TCON(5),  NULL, 1 },
    { "TF1",    NULL, 0, 1, LABEL, TCON(7),  NULL, 1 },
    { "TF2",    NULL, 0, 1, LABEL, T2CON(7), NULL, 1 },
    { "TH0",    NULL, 0, 1, LABEL, 0x8C,     NULL, 1 },
    { "TH1",    NULL, 0, 1, LABEL, 0x8D,     NULL, 1 },
    { "TH2",    NULL, 0, 1, LABEL, 0xCD,     NULL, 1 },
    { "TI",     NULL, 0, 1, LABEL, SCON(1),  NULL, 1 },
    { "TL0",    NULL, 0, 1, LABEL, 0x8A,     NULL, 1 },
    { "TL1",    NULL, 0, 1, LABEL, 0x8B,     NULL, 1 },
    { "TL2",    NULL, 0, 1, LABEL, 0xCC,     NULL, 1 },
    { "TMOD",   NULL, 0, 1, LABEL, 0x89,     NULL, 1 },
    { "TR0",    NULL, 0, 1, LABEL, TCON(4),  NULL, 1 },
    { "TR1",    NULL, 0, 1, LABEL, TCON(6),  NULL, 1 },
    { "TR2",    NULL, 0, 1, LABEL, T2CON(2), NULL, 1 },
    { "TXD",    NULL, 0, 1, LABEL, P3(1),    NULL, 1 },
    { "WR",     NULL, 0, 1, LABEL, P3(6),    NULL, 1 }
};

#define SINITSIZE   (sizeof(sinit)/sizeof(sinit[0]))

/* ----------------------------------------------------------------------
 * opcode vectors:
 *  These arrays contain the various opcodes for the
 *  various forms an instruction may take.
 *
 *  The ordering of these opcodes is very critical to the
 *  proper fuctioning of the assembler.
 *
 *  When a given form of an instruction is parsed, the parser
 *  indexes one of these arrays by the correct amount and thus
 *  obtains the correct opcode for the particular form.
 *
 */
#define STUNCHAR static unsigned char

STUNCHAR i_acall[]={ 0x11 };
STUNCHAR i_add[]=  { 0x28, 0x25, 0x26, 0x24 };
STUNCHAR i_addc[]= { 0x38, 0x35, 0x36, 0x34 };
STUNCHAR i_ajmp[]= { 0x01 };
STUNCHAR i_anl[]=  { 0x58, 0x55, 0x56, 0x54, 0x52, 0x53, 0x82, 0xb0 };
STUNCHAR i_cjne[]= { 0xb5, 0xb4, 0xb8, 0xb6 };
STUNCHAR i_clr[]=  { 0xe4, 0xc3, 0xc2 };
STUNCHAR i_cpl[]=  { 0xf4, 0xb3, 0xb2 };
STUNCHAR i_da[]=   { 0xd4 };
STUNCHAR i_dec[]=  { 0x14, 0x18, 0x15, 0x16 };
STUNCHAR i_div[]=  { 0x84 };
STUNCHAR i_djnz[]= { 0xd8, 0xd5 };
STUNCHAR i_inc[]=  { 0x04, 0x08, 0x05, 0x06, 0xa3 };
STUNCHAR i_jb[]=   { 0x20 };
STUNCHAR i_jbc[]=  { 0x10 };
STUNCHAR i_jc[]=   { 0x40 };
STUNCHAR i_jmp[]=  { 0x73 };
STUNCHAR i_jnb[]=  { 0x30 };
STUNCHAR i_jnc[]=  { 0x50 };
STUNCHAR i_jnz[]=  { 0x70 };
STUNCHAR i_jz[]=   { 0x60 };
STUNCHAR i_lcall[]={ 0x12 };
STUNCHAR i_ljmp[]= { 0x02 };
STUNCHAR i_mov[]=  { 0xe8, 0xe5, 0xe6, 0x74, 0xf5, 0x75, 0xf8,
                     0xa8, 0x78, 0x88, 0x85, 0x86, 0xf6, 0xa6,
                     0x76, 0x90, 0xa2, 0x92 };
STUNCHAR i_movc[]= { 0x93, 0x83 };
STUNCHAR i_movx[]= { 0xe2, 0xe3, 0xe0, 0xf2, 0xf3, 0xf0 };
STUNCHAR i_mul[]=  { 0xa4 };
STUNCHAR i_nop[]=  { 0x00 };
STUNCHAR i_orl[]=  { 0x48, 0x45, 0x46, 0x44, 0x42, 0x43, 0x72,
                     0xa0 };
STUNCHAR i_pop[]=  { 0xd0 };
STUNCHAR i_push[]= { 0xc0 };
STUNCHAR i_ret[]=  { 0x22 };
STUNCHAR i_reti[]= { 0x32 };
STUNCHAR i_rl[]=   { 0x23 };
STUNCHAR i_rlc[]=  { 0x33 };
STUNCHAR i_rr[]=   { 0x03 };
STUNCHAR i_rrc[]=  { 0x13 };
STUNCHAR i_setb[]= { 0xd3, 0xd2 };
STUNCHAR i_sjmp[]= { 0x80 };
STUNCHAR i_subb[]= { 0x98, 0x95, 0x96, 0x94 };
STUNCHAR i_swap[]= { 0xc4 };
STUNCHAR i_xch[]=  { 0xc8, 0xc5, 0xc6 };
STUNCHAR i_xchd[]= { 0xd6 };
STUNCHAR i_xrl[]=  { 0x68, 0x65, 0x66, 0x64, 0x62, 0x63 };

/* ----------------------------------------------------------------------
 * optable[]
 *  This table contains opcodes, directives and a few reserved
 *  symbols.
 *
 *  The second field is the keywords token value.
 *
 *  Unless the symbol is an opcode, the third field will
 *  be NULL.
 * 
 *  The third field is a pointer to an array of opcode bytes.
 *
 *    --> This list must be in alphabetical order by the first field
 */

static struct opcode optable[] = {
    {"a",       A,      NULL    },
    {"ab",      AB,     NULL    },
    {"acall",   ACALL,  i_acall },
    {"add",     ADD,    i_add   },
    {"addc",    ADDC,   i_addc  },
    {"ajmp",    AJMP,   i_ajmp  },
    {"and",     AND,    NULL    },
    {"anl",     ANL,    i_anl   },
    {"ar0",     AR0,    NULL    },
    {"ar1",     AR1,    NULL    },
    {"ar2",     AR2,    NULL    },
    {"ar3",     AR3,    NULL    },
    {"ar4",     AR4,    NULL    },
    {"ar5",     AR5,    NULL    },
    {"ar6",     AR6,    NULL    },
    {"ar7",     AR7,    NULL    },
    {"at",      D_AT,   NULL    },
    {"bit",     D_BIT,  NULL    },
    {"bseg",    D_BSEG, NULL    },
    {"byte",    D_BYTE, NULL    },
    {"c",       C,      NULL    },
    {"call",    CALL,   i_acall },
    {"cjne",    CJNE,   i_cjne  },
    {"clr",     CLR,    i_clr   },
    {"code",    D_CODE, NULL    },
    {"cpl",     CPL,    i_cpl   },
    {"cseg",    D_CSEG, NULL    },
    {"da",      DA,     i_da    },
    {"data",    D_DATA, NULL    },
    {"db",      D_BYTE, NULL    },
    {"dbit",    D_DBIT, NULL    },
    {"dec",     DEC,    i_dec   },
    {"div",     DIV,    i_div   },
    {"djnz",    DJNZ,   i_djnz  },
    {"dptr",    DPTR,   NULL    },
    {"ds",      D_DS,   NULL    },
    {"dseg",    D_DSEG, NULL    },
    {"dw",      D_WORD, NULL    },
    {"else",    ELSE,   NULL    },
    {"end",     D_END,  NULL    },
    {"endif",   ENDIF,  NULL    },
    {"eq",      C_EQ,   NULL    },
    {"equ",     D_EQU,  NULL    },
    {"extern",  EXTERN, NULL    },
    {"extrn",   EXTERN, NULL    },
    {"ge",      C_GE,   NULL    },
    {"gt",      C_GT,   NULL    },
    {"high",    HIGH,   NULL    },
    {"idata",   D_IDATA,NULL    },
    {"if",      IF,     NULL    },
    {"ifdef",   IFDEF,  NULL    },
    {"ifndef",  IFNDEF, NULL    },
    {"inc",     INC,    i_inc   },
    {"iseg",    D_ISEG, NULL    },
    {"jb",      JB,     i_jb    },
    {"jbc",     JBC,    i_jbc   },
    {"jc",      JC,     i_jc    },
    {"jmp",     JMP,    i_jmp   },
    {"jnb",     JNB,    i_jnb   },
    {"jnc",     JNC,    i_jnc   },
    {"jnz",     JNZ,    i_jnz   },
    {"jz",      JZ,     i_jz    },
    {"lcall",   LCALL,  i_lcall },
    {"le",      C_LE,   NULL    },
    {"ljmp",    LJMP,   i_ljmp  },
    {"low",     LOW,    NULL    },
    {"lt",      C_LT,   NULL    },
    {"mac",     D_MAC,  NULL    },
    {"mov",     MOV,    i_mov   },
    {"movc",    MOVC,   i_movc  },
    {"movx",    MOVX,   i_movx  },
    {"mul",     MUL,    i_mul   },
    {"ne",      C_NE,   NULL    },
    {"nop",     NOP,    i_nop   },
    {"not",     NOT,    NULL    },
    {"or",      OR,     NULL    },
    {"org",     D_ORG,  NULL    },
    {"orl",     ORL,    i_orl   },
    {"overlay", OVERL,  NULL    },
    {"pc",      PC,     NULL    },
    {"pop",     POP,    i_pop   },
    {"public",  PUBLIC, NULL    },
    {"push",    PUSH,   i_push  },
    {"r0",      R0,     NULL    },
    {"r1",      R1,     NULL    },
    {"r2",      R2,     NULL    },
    {"r3",      R3,     NULL    },
    {"r4",      R4,     NULL    },
    {"r5",      R5,     NULL    },
    {"r6",      R6,     NULL    },
    {"r7",      R7,     NULL    },
    {"ret",     RET,    i_ret   },
    {"reti",    RETI,   i_reti  },
    {"rl",      RL,     i_rl    },
    {"rlc",     RLC,    i_rlc   },
    {"rr",      RR,     i_rr    },
    {"rrc",     RRC,    i_rrc   },
    {"rseg",    D_RSEG, NULL    },
    {"seg",     D_SEG,  NULL    },
    {"segment", D_SEG,  NULL    },
    {"set",     D_SET,  NULL    },
    {"setb",    SETB,   i_setb  },
    {"shl",     SHL,    NULL    },
    {"shr",     SHR,    NULL    },
    {"sjmp",    SJMP,   i_sjmp  },
    {"skip",    D_SKIP, NULL    },
    {"subb",    SUBB,   i_subb  },
    {"swap",    SWAP,   i_swap  },
    {"using",	USING,  NULL    },
    {"word",    D_WORD, NULL    },
    {"xch",     XCH,    i_xch   },
    {"xchd",    XCHD,   i_xchd  },
    {"xdata",   D_XDATA,NULL    },
    {"xor",     XOR,    NULL    },
    {"xrl",     XRL,    i_xrl   },
    {"xseg",    D_XSEG, NULL    }
};

#define OPTABSIZE   (sizeof(optable)/sizeof(struct opcode))

/* ----------------------------------------------------------------------
 * lookop:
 *  Do a binary search through optable[], for a matching
 *  symbol. Return the symbol found or NULL.
 *
 */

struct opcode *lookop(const char *s)
{
    register int low,high,mid,cond;

    low = 0;
    high = OPTABSIZE-1;
    while( low<=high ) {
        mid = (low+high)/2;
        if( (cond = strcasecmp(s,optable[mid].name)) < 0 )
            high = mid-1;
        else if(cond > 0 )
            low = mid+1;
        else
            return(&optable[mid]);
    }
    return(NULL);
}

/* ----------------------------------------------------------------------
 * symtab, hash, looksym:
 *  User symbol table routines.
 *  symtab is the hash table for the symbols.
 *  (chaining is used for collision resolution).
 *
 */

static struct symbol *symtab[HASHTABSIZE];

int hash(const char *s)
{
    register const char *p;
    register unsigned h=0, g;

    for (p=s; *p; p++)
	{
		if(CaseSensitive_Flag!=1)
		{
			h = (h<<4) + toupper(*p);
		}
		else
		{
			h = (h<<4) + (*p);
		}
        if ( (g = h&0xf0000000) )
		{
            h = h ^ (g >> 24);
            h = h ^ g;
        }
    }
    return( h % HASHTABSIZE );
}

struct symbol *looksym(const char *s)
{
    register struct symbol *ptr, *prev;
    char *p;
    register int hv;
	int compare_result;

    hv = hash(s);

    prev = NULL;
    for(ptr=symtab[hv]; ptr; ptr = ptr->next)
	{
		if (CaseSensitive_Flag!=1)
			compare_result=!strcasecmp(ptr->name,s);
		else
			compare_result=!strcmp(ptr->name,s);

        if( compare_result )
		{
            if( prev != NULL )
			{
                prev->next = ptr->next;
                ptr->next = symtab[hv];
                symtab[hv] = ptr;
            }
			ptr->used++;
            return(ptr);
        }
        prev = ptr;
    }

    if ( (p = malloc(strlen(s) + 1)) )
	{
        strcpy(p,s);
    }
	else
	{
        fatalerror("Cannot allocate %d bytes",strlen(s)+1);
    }

    ptr = (struct symbol *) malloc( sizeof(struct symbol) );
    if( ptr == NULL )
        fatalerror("Cannot allocate %d bytes",sizeof(struct symbol));
    ptr->name = p;
	ptr->literal = NULL;
    ptr->flags = 0;
    ptr->type = UNDEF;
    ptr->next = symtab[hv];
	ptr->used = 0;
	ptr->segment =  NULL;
    symtab[hv] = ptr;
    return(ptr);
}

/* ----------------------------------------------------------------------
 * syminit:
 *  Initializes the hash table, with the initial symbols from
 *  sinit[]
 *
 */

// static struct symbol *symtab[HASHTABSIZE];

void syminit(void)
{
	#define BUFFSIZE 0x200
    int i, j;
    struct symbol * my_symbol;
	char buff[BUFFSIZE];

    /* load all the pre-defined symbols */
    for(i=0; i<SINITSIZE; i++ )
    {
        my_symbol=looksym(sinit[i].name);
        my_symbol->literal=sinit[i].literal;
        my_symbol->type=sinit[i].type;
        my_symbol->value=sinit[i].value;
        my_symbol->size=sinit[i].size;
		my_symbol->flags=CONSTANT;

		if(CaseSensitive_Flag==1) // Create also the lowercase version of predefined symbol
		{
			if(strlen(sinit[i].name)<(BUFFSIZE-1))
			{
				for(j=0; sinit[i].name[j]!=0; j++) buff[j]=tolower(sinit[i].name[j]);
				buff[j]=0;
				my_symbol=looksym(buff);
				my_symbol->literal=sinit[i].literal;
				my_symbol->type=sinit[i].type;
				my_symbol->value=sinit[i].value;
				my_symbol->size=sinit[i].size;
				my_symbol->flags=CONSTANT;
			}
		}
    }
}

void seginit (void)
{
	abs_cseg=looksym("CSEG");
    abs_cseg->literal=NULL;
    abs_cseg->type=CSEG;
    abs_cseg->value=0;
    abs_cseg->size=0;
	abs_cseg->flags=SEGMENT;
	last_cseg=abs_cseg;

	current_seg=abs_cseg;

	abs_dseg=looksym("DSEG");
    abs_dseg->literal=NULL;
    abs_dseg->type=DSEG;
    abs_dseg->value=0;
    abs_dseg->size=0;
	abs_dseg->flags=SEGMENT;

	abs_iseg=looksym("ISEG");
    abs_iseg->literal=NULL;
    abs_iseg->type=ISEG;
    abs_iseg->value=0;
    abs_iseg->size=0;
	abs_iseg->flags=SEGMENT;

	abs_xseg=looksym("XSEG");
    abs_xseg->literal=NULL;
    abs_xseg->type=XSEG;
    abs_xseg->value=0;
    abs_xseg->size=0;
	abs_xseg->flags=SEGMENT;

	abs_bseg=looksym("BSEG");
    abs_bseg->literal=NULL;
    abs_bseg->type=BSEG;
    abs_bseg->value=0;
    abs_bseg->size=0;
	abs_bseg->flags=SEGMENT;
}

/* Clear all symbols */
void clearsym (void)
{
    /* clear all entries in the symbol table */
    memset(symtab, 0, sizeof(struct symbol *) * HASHTABSIZE);
}

/* free all the memory allocated for the symbols */

void freesym(void)
{
    struct symbol *sym, *next;
    int i;

    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED)
			{
                sym = sym->next;
            }
			else
			{
                if (sym->name) free(sym->name);
                if (sym->literal) free(sym->literal);
                next = sym->next;
                free(sym);
                sym = next;
            }
        }
    }
}

void reset_symbol_usage_counter(void)
{
    struct symbol *sym, *next;
    int i;

    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED)
			{
                sym = sym->next;
            }
			else
			{
                sym->used=0;
                next = sym->next;
                sym = next;
            }
        }
    }
}

void make_undeclared_symbols_external(void)
{
    struct symbol *sym, *next;
    int i;

    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED)
			{
                sym = sym->next;
            }
			else
			{
				if((sym->name!=NULL) && (sym->type==UNDEF))
				{
					//printf("Made %s external\n", sym->name);
					sym->flags=EXTERN_SYM;
					sym->type=0x80; //generic symbol.  We don't know until we link.
					sym->size=0;
					sym->value=0;
				}
                next = sym->next;
                sym = next;
            }
        }
    }
}

void print_object(FILE * fobj)
{
    struct symbol *sym, *next;
    int i;
	char * str;

	fprintf(fobj, "<SEGMENTS>\n");
    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED) sym = sym->next;
			else
			{
				if( sym->type!=UNDEF )
				{
					if(sym->flags&RELOCATABLE)
					{
						switch( sym->type )
						{
							case CSEG: str="code";   break;
							case DSEG: str="data";   break;
							case ISEG: str="idata";  break;
							case BSEG: str="bit";    break;
							case XSEG: str="xdata";  break;
							default:   str="ERROR!"; break;
						}
						fprintf(fobj, "%s,%s,%04X,%s\n", sym->name, str, sym->value, (sym->flags&OVERLAYABLE)?"OV":"NO");
					}
				}
				next = sym->next;
                sym = next;
            }
        }
    }
	fprintf(fobj, "</SEGMENTS>\n\n");

	fprintf(fobj, "<LOCALS>\n");
    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED) sym = sym->next;
			else
			{
				if(sym->segment!=NULL)
				{
					if ( (sym->segment->flags&RELOCATABLE) ||
						 (sym->type==DSEG) ||
						 (sym->type==ISEG) ||
						 (sym->type==XSEG) ||
						 (sym->type==BSEG) )
					{
						if( (sym->flags==0) ) // Not a public or external symbol
							fprintf(fobj, "%s,%s,%04X,%04X\n", sym->name, sym->segment->name, sym->value, sym->size);
					}
				}
                next = sym->next;
                sym = next;
            }
        }
    }
	fprintf(fobj, "</LOCALS>\n\n");

	fprintf(fobj, "<PUBLICS>\n");
    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED) sym = sym->next;
			else
			{
				if( sym->type!=UNDEF )
				{
					if( sym->flags==PUBLIC_SYM )
						fprintf(fobj, "%s,%s,%04X,%04X\n", sym->name, sym->segment->name, sym->value, sym->size);
				}
                next = sym->next;
                sym = next;
            }
        }
    }
	fprintf(fobj, "</PUBLICS>\n\n");
	
	fprintf(fobj, "<EXTERNALS>\n");
    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
            if (sym->flags&PREDEFINED) sym = sym->next;
			else
			{
				if(sym->flags==EXTERN_SYM)
				{
					switch( sym->type )
					{
						case CSEG: str="code";   break;
						case DSEG: str="data";   break;
						case ISEG: str="idata";  break;
						case BSEG: str="bit";    break;
						case XSEG: str="xdata";  break;
						default:   str="any";    break;
					}
					fprintf(fobj, "%s,%s,%04X,%04X\n", sym->name, str, sym->value, sym->size);
				}
                next = sym->next;
                sym = next;
            }
        }
    }
	fprintf(fobj, "</EXTERNALS>\n\n");
}

void doclean (char * buffer)
{
	int j;
	for(j=0; buffer[j]!=0; j++)
	{
		if( (buffer[j]=='\r') || (buffer[j]=='\n') )
		{
			buffer[j]=0;
			return;
		}
		else if ( (buffer[j]=='=') || (buffer[j]==':') || (buffer[j]==',') )
		{
			buffer[j]=' ';
		}

	}
}

// This function parses a MAP file generated by the linker and assings values to
// relocatable local, public, and external symbols.  It should be called just before
// the second pass of the assembler.
void LoadSymbolValues (char * MapFile, char * modname)
{
	char buffer[0x1000];
	char s1[0x100], s2[0x100], s3[0x100];
	FILE * infile;
    struct symbol *sym, *next;
	int value;
    int i;
	int state=0; // 0: nothing,  1: Segments, 2: Public symbols, 3: Local symbols

    for (i=0; i<HASHTABSIZE; i++)
	{
        sym = symtab[i];
        while (sym != NULL)
		{
			if(sym->flags&RELOCATABLE)
			{
				sym->value=0;
			}

			next = sym->next;
            sym = next;
        }
    }

	infile = fopen(MapFile, "r");
    if (infile == NULL)
    {
        mesg_f("WARNING: Cannot open input file '%s'.  Relisting aborted for '%s'.\n", MapFile, modname);
        //return;
		exit(1);
    }

	// printf("Module name: %s\n", modname);

	while(fgets(buffer, sizeof(buffer), infile)!=NULL)
	{
		doclean(buffer);

		// Initialize some valiables used with scanf below
		s1[0]=0; s2[0]=0; s3[0]=0; value=0;

		if(strcmp("Module Segments ", buffer)==0) state=1;
		else if(strcmp("Public symbols ", buffer)==0) state=2;
		else if(strcmp("Local symbols ", buffer)==0) state=3;
		else switch (state)
		{
			case 1:
				// example: tetris:BIT_BANK=D:0020, size=0001
				sscanf(buffer,"%254s %254s %254s %X", s1, s2, s3, &value);
				if(strcmp(s1, modname)==0)
				{
					//printf("%s:%s=%s:%04X\n", s1, s2, s3, value);
					sym = looksym(s2);

					if(sym->type == UNDEF)
					{
						printf("Symbol %s not found\n", sym->name);
					}
					sym->value=value;
				}
				break;
			case 2:
				//example: _dummyint3=C:036A
				sscanf(buffer,"%254s %254s %X", s1, s2, &value);
				if(s2[0]!='L')
				{
					if(strlen(s1)>0) // Make sure to skip empty lines
					{
						sym = looksym(s1);
						//printf("%s=%04X\n", s1, value);
						sym->value=value;
					}
				}
				break;
			case 3:
				//example: tetris:L002002?=C:0320
				sscanf(buffer,"%254s %254s %254s %X", s1, s2, s3, &value);
				if(strcmp(s1, modname)==0)
				{
					sym = looksym(s2);
					//printf("%s:%s=%s:%04X\n", s1, s2, s3, value);
					if(sym->type == UNDEF)
					{
						printf("Symbol %s not found\n", sym->name);
					}
					sym->value=value;
				}
				break;
			default:
				break;
		}
	}

	fclose(infile);
}

