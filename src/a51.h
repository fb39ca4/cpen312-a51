/*  a51.h

    Copyright (C) 2010 - 2012  Jesus Calvino-Fraga, jesusc (at) ece.ubc.ca
    
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

/* ----------------------------------------------------------------------
 * user / keyword symbol structures:
 */

struct opcode {
	char *name;
	int type;
	unsigned char *bytes;
};

struct symbol {
	char *name;
	char *literal; //For EQU, SET and MAC
	unsigned int used; //How many times this symbol has been used
	unsigned char flags;
	unsigned char type;
	unsigned long value;
	struct symbol *next;
	unsigned int size; // Size in bytes this symbol represents (for DSEG, ISEG, XSEG, and BSEG)
	struct symbol *segment;
};

// Symbol types:
#define UNDEF	0
#define LABEL	1
#define CSEG    2
#define DSEG    3
#define ISEG    4
#define BSEG    5
#define XSEG    6
#define EQU     7
#define SET     8
#define MAC     9
#define RSEG   10

// Symbol flags
#define PLAIN_SYM   0x00
#define PREDEFINED  0x01
#define CONSTANT    0x02
#define SEGMENT     0x04
#define RELOCATABLE 0x08
#define OVERLAYABLE 0x10
#define EXTERN_SYM  0x20
#define PUBLIC_SYM  0x40

/* ----------------------------------------------------------------------
 * addressing mode stuff:
 */

struct mode {
	unsigned char mode;		/* value to index with */
	unsigned char size;		/* # of bytes used */
	unsigned char orval;	/* value OR'd to opcode */
	int byte1;	/* extra byte 1 */
	char str1[0x100];
	int byte2;	/* extra byte 2 */
	char str2[0x100];
};

#define set_md(m,a)	((m).mode=(a))
#define set_sz(m,a)	((m).size=(a))
#define set_ov(m,a)	((m).orval=(unsigned char)(a))
#define set_b1(m,a)	((m).byte1=(a))
#define set_b2(m,a)	((m).byte2=(a))

#define get_md(m)	((m).mode)
#define get_sz(m)	((m).size)
#define get_ov(m)	((m).orval)
#define get_b1(m)	((m).byte1)
#define get_b2(m)	((m).byte2)

/* ----------------------------------------------------------------------
 * yacc stack stuff:
 */

struct value {
	long v;
	unsigned char d;		/* expression defined flag */
	unsigned char s;        /* relocatable expression flag*/
	unsigned char k;        /* a constant was passed if zero*/
	char rel[0x1000];       /* relocatable expression value */
};

union ystack {
	long value;
	struct value val;
	struct opcode *op;
	struct symbol *sym;
	struct mode mode;
	char *str;
};

#define YYSTYPE union ystack

/* ----------------------------------------------------------------------
 * IS_BIT_MAPPED_RAM:
 *	True is the byte 'a' is the byte address of a bit mapped
 *	RAM location.
 */
#define isbmram(a)	(((a)&0xf0)==0x20)

/* ----------------------------------------------------------------------
 * IS_BIT_MAPPED_SFR:
 *	True is the byte 'a' is the byte address of a bit mapped
 *	SPECIAL FUCTION REGISTER.
 */
#define isbmsfr(a)	(((a)&0x80) && !((a) & 0x07))

/* ----------------------------------------------------------------------
 * isbit8, ...
 *	Macros to check the sizes of values and to convert
 *	a value to a certain, size.
 *
 */
#define size8		(~0x00ff)
#define size11		(~0x07ff)
#define size13		(~0x1fff)
#define size16		(~0xffff)

#define size10		(~0x03ff)
#define size12		(~0x0fff)
#define size15		(~0x7fff)

#define isbit8(v)	( !( ((v)>=0) ? (v)&size8 : -(v)>=128) )
#define isbit11(v)	( !( ((v)>=0) ? (v)&size11 : (-(v))&size10 ) )
#define isbit13(v)	( !( ((v)>=0) ? (v)&size13 : (-(v))&size12 ) )
#define isbit16(v)	( !( ((v)>=0) ? (v)&size16 : (-(v))&size15 ) )

/* ----------------------------------------------------------------------
 * Size of user hash table.  Use a prime number for best performance
 */
#define HASHTABSIZE		4999

/* ----------------------------------------------------------------------
 * Macros to nicely test which pass we are in.
 */
#define pass1			(!pass)
#define pass2			(pass)

/* from lexer.c */
extern int yylex(void);
extern int seek_eol(void);
extern const char *get_last_token(void);
extern int lineno;
void add_ch(char c);

/* from parser.y */
extern int yyparse(void);
extern int RegisterBank;

/* from emitter.c */
extern void emitusage(void);
extern const char *emit_extension(const char *ftype);
extern const char *emit_desc_lookup(int num);
extern const char *emit_desc_to_name_lookup(const char *desc);
extern int emitopen(const char *file, const char *ftype, const char *arg);
extern void emitclose(void);
extern void emitaddr(unsigned long a);
extern void emitbyte(int b);

/* from symbol.c */
extern struct opcode *lookop(const char *s);
extern struct symbol *looksym(const char *s);
extern void syminit(void);
extern void freesym(void);
extern void clearsym (void);
extern void reset_symbol_usage_counter(void);
extern void make_undeclared_symbols_external(void);
extern void print_object(FILE * fobj);
extern void seginit (void);
extern void LoadSymbolValues (char * MapFile, char * modname);
extern struct symbol * abs_cseg;
extern struct symbol * abs_dseg;
extern struct symbol * abs_iseg;
extern struct symbol * abs_xseg;
extern struct symbol * abs_bseg;
extern struct symbol * current_seg;
extern struct symbol * last_cseg;

/* from run.c */
extern int run_a51(const char *infile, int lst, int use_stdout,
        const char *fmt, const char *arg);
extern void push_include(const char *infile);
void pop_include(void);
extern void fatalerror(const char *fmt, ...);
extern void error(const char *fmt, ...);
extern void warning(const char *fmt, ...);
extern void mesg_f(const char *fmt, ...);
extern int dashl;
extern int pass;
extern int abort_asap;
extern int dosyminit;
extern FILE *listing;
extern int ObjectFile;
extern FILE * fobj;

/* from a51.c */
extern void mesg(const char *str);

/* from pathtools.c */
void mysplitpath(const char *path, char *drive, char *dir, char *fName, char *ext);
void mymakepath(char *path, const char *drive, const char *dir, const char *fName, const char *ext);

#define _makepath mymakepath
#define _splitpath mysplitpath

#if defined(_MSC_VER)
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#define vsnprintf _vsnprintf
#define snprintf _snprintf
#pragma warning (disable : 4996)
// Add _CRT_SECURE_NO_WARNINGS to the project instead of the pragma above
#endif

#if defined(_WIN64)
#pragma warning (disable : 4244)
#pragma warning (disable : 4267)
#endif

#if !defined(PATH_MAX) || (PATH_MAX < 2048)
#  undef  PATH_MAX
#  define PATH_MAX 2048         /* define a reasonable value */
#endif

#if !defined(_MAX_DRIVE)
#	define _MAX_DRIVE PATH_MAX
#endif

#if !defined(_MAX_DIR)
#	define _MAX_DIR PATH_MAX
#endif

#if !defined(_MAX_FNAME)
#	define _MAX_FNAME PATH_MAX
#endif

#if !defined(_MAX_EXT)
#	define _MAX_EXT PATH_MAX
#endif
