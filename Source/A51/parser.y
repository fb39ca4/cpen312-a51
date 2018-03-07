/*  parser.y

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

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "a51.h"

#define YYSTYPE union ystack

static unsigned char kmask[]={
	0x01, 0x02, 0x04, 0x08,	0x10, 0x20, 0x40, 0x80,
	0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01};

static unsigned char bytebuf[0x4000]; /* used by dumplist() */
struct symbol * label_symbol = NULL;

char symexp[0x100];
char tmpexp[0x100];

char optc51[0x1000]=" ";
extern char MapFile[PATH_MAX];

int printf_float=0;

static int bytecount;
int RegisterBank=0;

int IF_Flag[0x100];
int IF_Level;
char IF_Value[0x1000];
int IF_Count;
int force_EOF;
int hash_warning_enable=1; // Just after adding this feature I found two bugs in old code!
extern int linetest;

void yyerror(char *s);
int makeop(struct opcode * op, struct mode *m, int add);
char *padline(char *line);
void dumplist(char *txt, int show);
void genbyte(int b);
void genstr(char *s);
void genword(unsigned long w);
void BadRegUse(char * x);
void objbyte(int b);
void objstr(char *s);
void objword(unsigned long w);
void set_str1(struct mode *m, union ystack *y);
void set_str2(struct mode *m, union ystack *y);
void NoObjectFileError (char * x);

/* ------------------------ G R A M M E R ----------------------------- */

%}

%token STRING
%token D_ORG
%token D_BYTE
%token D_WORD
%token D_SKIP
%token D_EQU
%token D_SET
%token D_MAC
%token D_DBIT
%token D_END
%token D_DSEG
%token D_ISEG
%token D_BSEG
%token D_CSEG
%token D_XSEG
%token ACALL
%token ADD
%token ADDC
%token AJMP
%token ANL
%token CALL
%token CJNE
%token CLR
%token CPL
%token DA
%token DEC
%token DIV
%token DJNZ
%token INC
%token D_INCLUDE
%token JB
%token JBC
%token JC
%token JMP
%token JNB
%token JNC
%token JNZ
%token JZ
%token LCALL
%token LJMP
%token MOV
%token MOVC
%token MOVX
%token NOP
%token MUL
%token ORL
%token POP
%token PUSH
%token RET
%token RETI
%token RL
%token RLC
%token RR
%token RRC
%token SETB
%token SJMP
%token SUBB
%token SWAP
%token XCH
%token XCHD
%token XRL
%token AB
%token A
%token C
%token PC
%token DPTR
%token BITPOS
%token R0
%token R1
%token R2
%token R3
%token R4
%token R5
%token R6
%token R7
%token USING
%token AR0
%token AR1
%token AR2
%token AR3
%token AR4
%token AR5
%token AR6
%token AR7
%token VALUE
%token SYMBOL
%token CONTROL
%token HIGH
%token LOW
%token NOT
%token D_AT
%token D_DS
%token D_BIT
%token D_DATA
%token D_XDATA
%token D_IDATA
%token D_CODE
%token MOD
%token D_SEG
%token D_RSEG
%token PUBLIC
%token EXTERN
%token OVERL
%token IF
%token IFDEF
%token IFNDEF
%token ENDIF
%token ELSE
%token ERROR

%left '+' '-'
%left '*' '/' '%'
%left '|' '&' '^'
%left  OR AND XOR
%left '>' '<'
%left '=' '!'
%left SHR SHL
%left HIGH LOW NOT
%left C_GT C_LT C_GE C_LE C_EQ C_NE

%start program

%%

program:
	linelist

;

linelist:
	linelist line
	| line
;

line:
	undefsym ':' D_DS defexpr linerest
		{
			if($4.val.v<0)
			{
				error("size in DS directive can not be negative");
			}

			if ( ( (current_seg->type==DSEG) || (current_seg->type==ISEG) ) 
				  && (ObjectFile==0) && (current_seg->value<8) )
			{
				if(pass1) warning("DS directive overlaps register bank 0");
			}

			label_symbol=$1.sym;

			if ( (current_seg->type==DSEG) || (current_seg->type==ISEG) ||
				 (current_seg->type==XSEG) )
			{
				$1.sym->value = current_seg->value;
				$1.sym->flags = 0;
				current_seg->value+=$4.val.v;
				$1.sym->type = current_seg->type;
				$1.sym->size = $4.val.v;
				$1.sym->segment=current_seg;
			}
			else
			{
				error("DS directive works only in DSEG, ISEG, or XSEG");
			}
			bytecount = 0;
			$$.value = 0;
		}
	| D_DS defexpr linerest
		{
			if($2.val.v<0)
			{
				error("size in DS directive can not be negative");
			}

			if ( ( (current_seg->type==DSEG) || (current_seg->type==ISEG) ) 
				  && (ObjectFile==0) && (current_seg->value<8) )
			{
				if(pass1) warning("DS directive overlaps register bank 0");
			}

			if(label_symbol!=NULL)
			{
				if(current_seg->type==label_symbol->type)
				{
					if ( (current_seg->type==DSEG) || (current_seg->type==ISEG) ||
						 (current_seg->type==XSEG) )
					{
						current_seg->value+= $2.val.v;
						label_symbol->size += $2.val.v;
					}
					else
					{
						error("DS directive works only in DSEG, ISEG, or XSEG");
					}
				}
				else
				{
					current_seg->value+= $2.val.v;
					if(pass1) warning("DS directive without a preceding label");
				}
			}
			else
			{
				current_seg->value+= $2.val.v;
				if(pass1) warning("DS directive without a preceding label");
			}
			bytecount = 0;
			$$.value = 0;
		}
	| undefsym ':' D_DBIT defexpr linerest
		{
			label_symbol=$1.sym;
			
			if($4.val.v<0)
			{
				error("size in DBIT directive can not be negative");
			}
			if(current_seg->type!=BSEG)
			{
				error("DBIT directive works only in BSEG");
			}
			else
			{
				$1.sym->value = current_seg->value;
				$1.sym->flags = 0;
				current_seg->value+=$4.val.v;
				$1.sym->type = current_seg->type;
				$1.sym->size = $4.val.v;
				$1.sym->segment=current_seg;
			}
			bytecount = 0;
			$$.value = 0;
		}

	| D_DBIT defexpr linerest
		{
			if($2.val.v<0)
			{
				error("size in DBIT directive can not be negative");
			}
			if(label_symbol!=NULL)
			{
				if ( (label_symbol->type!=BSEG) || (current_seg->type!=BSEG) )
				{
					if(pass1) error("DBIT directive works only in BSEG");
				}
				else
				{
					current_seg->value+= $2.val.v;
					label_symbol->size += $2.val.v;
				}
			}
			else
			{
				current_seg->value+= $2.val.v;
				if(pass1) warning("DBIT directive without a preceding label");
			}
			bytecount = 0;
			$$.value = 0;
		}

	| undefsym ':' linerest
		{
			label_symbol=$1.sym;
			if (abort_asap)
			{
				YYABORT;
			}

			$1.sym->type = current_seg->type;
			$1.sym->value = current_seg->value;
			$1.sym->segment = current_seg;
			$1.sym->size = 0;

			if($1.sym->type==CSEG)
			{
				current_seg->value+=$3.value;
				bytecount = 0;
			}
		}
	| linerest
	{
		current_seg->value+=$1.value;
		bytecount = 0;
	}
;

linerest: 
	control '\n'
	{
		$$.value = $1.value;
		if( dashl && pass2 ) dumplist($2.str,1);
		symexp[0]=0; // start over with symbolic expression
	}
	|directive '\n'
	{
		$$.value = $1.value;
		if( dashl && pass2 ) dumplist($2.str,1);
		symexp[0]=0; // start over with symbolic expression
	}
	|conditional
	{
		$$.value = $1.value;
		symexp[0]=0; // start over with symbolic expression
	}
	|SYMBOL '('
	{
		if( ($1.sym->type == UNDEF) && pass1)
				error("Undefined Macro: %s",$1.sym->name);
		seek_eol();
		symexp[0]=0; // start over with symbolic expression
	}
	| instr '\n'
	{
		if (current_seg->type!=CSEG)
		{
			//Switch to last used code segment
			if(pass1) warning("%s set as the active segment.\n", last_cseg->name);  
			current_seg=last_cseg;
		}
		$$.value = $1.value;
		if( dashl && pass2 ) dumplist($2.str,1);
		symexp[0]=0; // start over with symbolic expression
		last_cseg=current_seg;
	}
	| '\n'
	{
		$$.value = 0;
		if( dashl && pass2 ) dumplist($1.str,0);
		symexp[0]=0; // start over with symbolic expression
	}
	| AB   { $$.value = 0; BadRegUse("AB");   }
	| A    { $$.value = 0; BadRegUse("A");    }
	| C    { $$.value = 0; BadRegUse("C");    }
	| PC   { $$.value = 0; BadRegUse("PC");   }
	| DPTR { $$.value = 0; BadRegUse("DPTR"); }
	| R0   { $$.value = 0; BadRegUse("R0");   }
	| R1   { $$.value = 0; BadRegUse("R1");   }
	| R2   { $$.value = 0; BadRegUse("R2");   }
	| R3   { $$.value = 0; BadRegUse("R3");   }
	| R4   { $$.value = 0; BadRegUse("R4");   }
	| R5   { $$.value = 0; BadRegUse("R5");   }
	| R6   { $$.value = 0; BadRegUse("R6");   }
	| R7   { $$.value = 0; BadRegUse("R7");   }
	| AR0  { $$.value = 0; BadRegUse("AR0");  }
	| AR1  { $$.value = 0; BadRegUse("AR1");  }
	| AR2  { $$.value = 0; BadRegUse("AR2");  }
	| AR3  { $$.value = 0; BadRegUse("AR3");  }
	| AR4  { $$.value = 0; BadRegUse("AR4");  }
	| AR5  { $$.value = 0; BadRegUse("AR5");  }
	| AR6  { $$.value = 0; BadRegUse("AR6");  }
	| AR7  { $$.value = 0; BadRegUse("AR7");  }
	| error // This is a tag name created by bison, not function error()!
	{
		seek_eol();
	}
;

control: CONTROL
{
	if(strncasecmp($1.str, "INCLUDE", 7)==0)
	{
		push_include($1.str);
	}
	else if(strncasecmp($1.str, "MOD", 3)==0)
	{
	    push_include($1.str);
		dosyminit=0; //Don't use default symbol names
	}
	else if(strncasecmp($1.str, "NOMOD51", 7)==0)
	{
		dosyminit=0; //Don't use default symbol names
	}
	else if ( (strncasecmp($1.str, "NOLIST", 6)==0) || (strncasecmp($1.str, "NOPRINT", 7)==0) )
	{
		dashl=0;
	}
	else if(strncasecmp($1.str, "PRINTF_FLOAT", 12)==0)
	{
		printf_float=1;
	}
	else if ( (strncasecmp($1.str, "LIST", 4)==0) || (strncasecmp($1.str, "PRINT", 5)==0) )
	{
		dashl=1;
	}
	else if(strncasecmp($1.str, "OPTC51", 6)==0)
	{
		// The options are passed to the linker to verify proper
		// intermodule compatibility when compiling with C51.
		int j;

		if(pass1)
		{
			strncpy(optc51, &$1.str[7], sizeof(optc51)-1);
			for(j=0; optc51[j]!=0; j++) // terminate string at first non-ASCII character
			{
				if( (optc51[j]<0x20) || (optc51[j]>0x7e) ) optc51[j]=0;
			}
		}

	}
	else if(strncasecmp($1.str, "ECHO", 4)==0)
	{
		if(pass2)
		{
			mesg_f("%s\n", &$1.str[5]);
		}
	}
	else if(strncasecmp($1.str, "MESSAGE", 7)==0)
	{
		if(pass2)
		{
			mesg_f("%s\n", &$1.str[8]);
		}
	}
	else if(strncasecmp($1.str, "ERROR", 5)==0)
	{
		if(pass2)
		{
			error("%s\n", &$1.str[6]);
		}
	}
	else if(strncasecmp($1.str, "WARNING", 7)==0)
	{
		if(pass2)
		{
			warning("%s\n", &$1.str[8]);
		}
	}
	else if(strncasecmp($1.str, "LINETEST", 8)==0)
	{
		linetest=atoi(&$1.str[8]);
	}
	else if( (strncasecmp($1.str, "POUNDWARNING", 12)==0) || (strncasecmp($1.str, "PW", 2)==0) )
	{
		hash_warning_enable=1;
	}
	else if( (strncasecmp($1.str, "NOPOUNDWARNING", 14)==0) || (strncasecmp($1.str, "NPW", 3)==0) )
	{
		hash_warning_enable=0;
	}
	else if( (strncasecmp($1.str, "HASHWARNING", 11)==0) || (strncasecmp($1.str, "HW", 2)==0) )
	{
		hash_warning_enable=1;
	}
	else if( (strncasecmp($1.str, "NOHASHWARNING", 13)==0) || (strncasecmp($1.str, "NHW", 3)==0) )
	{
		hash_warning_enable=0;
	}
	else if(strncasecmp($1.str, "NAME", 4)==0)
	{
		//The module name
	}
	else if(strncasecmp($1.str, "DATE", 4)==0)
	{
	}
	else if(strncasecmp($1.str, "DEBUG", 5)==0)
	{
	}
	else if(strncasecmp($1.str, "PAGELENGTH", 10)==0)
	{
	}
	else if(strncasecmp($1.str, "NOTABS", 6)==0)
	{
	}
	else if(strncasecmp($1.str, "NOPAGING", 8)==0)
	{
	}
	else if(strncasecmp($1.str, "NOSYMBOLS", 9)==0)
	{
	}
	else if(strncasecmp($1.str, "XREF", 4)==0)
	{
	}
	else if(strncasecmp($1.str, "NOMACRO", 7)==0)
	{
	}
	else if(strncasecmp($1.str, "NOBUILTIN", 9)==0)
	{
	}
	else if(strncasecmp($1.str, "TITLE", 5)==0)
	{
	}
	else if(strncasecmp($1.str, "EJECT", 5)==0)
	{
	}
	else if(strncasecmp($1.str, "NOGEN", 5)==0)
	{
	}
	else if(strncasecmp($1.str, "GENONLY", 7)==0)
	{
	}
	else if(strncasecmp($1.str, "GEN", 3)==0)
	{
	}
	else if(strncasecmp($1.str, "NOCOND", 6)==0)
	{
	}
	else if(strncasecmp($1.str, "CONDONLY", 8)==0)
	{
	}
	else if(strncasecmp($1.str, "COND", 4)==0)
	{
	}
	else if(strncasecmp($1.str, "SAVE", 4)==0)
	{
	}
	else if(strncasecmp($1.str, "RESTORE", 7)==0)
	{
	}
	else
		if (pass1) warning("Ignored unknown control '$%s'.\n", $1.str);

	free($1.str);
	$$.value = 0;
	add_ch('\n'); //Very important!
};

/* DIRECTIVES: */

directive:
	D_ORG defexpr
		{
			if(current_seg->flags&RELOCATABLE)
			{
				error("ORG directive is only allowed in absolute segments");
			}
			else
			{
				if(ObjectFile==0)
				{
					if(current_seg->value>(unsigned int)$2.val.v)
					{
						error("ORG directive must use a value that is greater than the current segment counter.");
					}
				}
				current_seg->value=$2.val.v;
				if( pass2 ) emitaddr(current_seg->value);
				bytecount = 0;
				$$.value = 0;
				if( (ObjectFile==1) && pass2 )
				{
					if(current_seg->type==CSEG)
					{
						fprintf(fobj, "</CODE>\n\n");
						fprintf(fobj, "<CODE AT %04X>\n", $2.val.v);
					}
				}
			}
		}
	| D_DSEG
		{
			label_symbol=NULL;
			current_seg=abs_dseg;
			$$.value = 0;
		}
	| D_DSEG D_AT defexpr
		{
			label_symbol=NULL;
			current_seg=abs_dseg;
			current_seg->value = $3.val.v;
			bytecount = 0;
			$$.value = 0;
		}
	| D_CSEG
		{
			label_symbol=NULL;
			if( (ObjectFile==1) && pass2 && (current_seg!=abs_cseg) )
			{
				fprintf(fobj, "</CODE>\n\n");
				fprintf(fobj, "<CODE AT %04X>\n", abs_cseg->value);
			}
			current_seg=abs_cseg;
			last_cseg=current_seg;
			if( pass2 ) emitaddr(current_seg->value);
			bytecount = 0;
			$$.value = 0;
		}
	| D_CSEG D_AT defexpr
		{
			label_symbol=NULL;
			if(ObjectFile==0)
			{
				if(current_seg->value>(unsigned int)$3.val.v)
				{
					error("'CSEG at' directive must use a value that is greater than the current CSEG counter.");
				}
			}
			if( (ObjectFile==1) && pass2 )
			{
				fprintf(fobj, "</CODE>\n\n");
				fprintf(fobj, "<CODE AT %04X>\n", $3.val.v);
			}
			current_seg=abs_cseg;
			last_cseg=current_seg;
			current_seg->value = $3.val.v;
			if( pass2 ) emitaddr(current_seg->value);
			bytecount = 0;
			$$.value = 0;
	}
	| D_ISEG
		{
			label_symbol=NULL;
			current_seg=abs_iseg;
			$$.value = 0;
		}
	| D_ISEG D_AT defexpr
		{
			label_symbol=NULL;
			current_seg=abs_iseg;
			current_seg->value = $3.val.v;
			bytecount = 0;
			$$.value = 0;
		}
	| D_BSEG
		{
			label_symbol=NULL;
			current_seg=abs_bseg;
			$$.value = 0;
		}
	| D_BSEG D_AT defexpr
		{
			label_symbol=NULL;
			current_seg=abs_bseg;
			current_seg->value = $3.val.v;
			bytecount = 0;
			$$.value = 0;
		}
	| D_XSEG
		{
			label_symbol=NULL;
			current_seg=abs_xseg;
			$$.value = 0;
		}
	| D_XSEG D_AT defexpr
		{
			label_symbol=NULL;
			current_seg=abs_xseg;
			current_seg->value = $3.val.v;
			bytecount = 0;
			$$.value = 0;
		}
	| D_BYTE blist
		{
			if(current_seg->type!=CSEG)
			{
				if(pass1) warning("%s set as the active segment.\n", last_cseg->name);  
				current_seg=last_cseg;
			}
			$$.value = $2.value;
			last_cseg=current_seg;
		}
	| D_WORD wlist
		{
			if(current_seg->type!=CSEG)
			{
				if(pass1) warning("%s set as the active segment.\n", last_cseg->name);  
				current_seg=last_cseg;
			}
			$$.value = $2.value;
			$$.val.s=$2.val.s;
			last_cseg=current_seg;
		}
	| D_SKIP defexpr
		{
			current_seg->value+=$2.val.v;
			$$.value = $2.val.v;
			if( pass2 )	emitaddr(current_seg->value);
		}
	| undefsym D_EQU
		{
			if(pass1)
			{
				$1.sym->literal=$2.str;
				$1.sym->type = EQU;
				$1.sym->value = 0;
			}
			else //pass2
			{
				free($2.str);
			}
			$$.value = 0;
		}
	| undefsym D_MAC
		{
			if(pass1)
			{
				$1.sym->literal=$2.str;
				$1.sym->type = MAC;
				$1.sym->value = 0;
			}
			else //pass2
			{
				free($2.str);
			}
			$$.value = 0;
		}
	| USING data8
		{
			if($2.val.v<4)
				RegisterBank=$2.val.v;
			else
				error("Register bank can only be set to 0, 1, 2, or 3");
			$$.value = 0;
		}
	| undefsym D_DATA data8
		{
			$1.sym->type = DSEG;
			$1.sym->segment = NULL;
			$1.sym->flags = CONSTANT; // These are not variables but constants
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_IDATA data8
		{
			$1.sym->type = ISEG;
			$1.sym->segment = NULL;
			$1.sym->flags = CONSTANT; // These are not variables but constants
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_XDATA data16
		{
			$1.sym->type = XSEG;
			$1.sym->segment = NULL;
			$1.sym->flags = CONSTANT; // These are not variables but constants
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_CODE data16
		{
			$1.sym->type = CSEG;
			$1.sym->segment = NULL;
			$1.sym->flags = CONSTANT; // These are not variables but constants
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_BIT bit
		{
			$1.sym->type = BSEG;
			$1.sym->segment = NULL;
			$1.sym->flags = CONSTANT; // These are not variables but constants
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_SET reg
		{
			if($1.sym->literal!=NULL) free($1.sym->literal);
			$1.sym->literal=(char *)malloc(2+1);
			sprintf($1.sym->literal, "R%d", $3.val.v);
			$1.sym->type = SET;
			$1.sym->value = 0;
			$$.value = 0;
		}
	| undefsym D_SET areg
		{
			if($1.sym->literal!=NULL) free($1.sym->literal);
			$1.sym->literal=(char *)malloc(3+1);
			sprintf($1.sym->literal, "AR%d", $3.val.v);
			$1.sym->type = SET;
			$1.sym->value = 0;
			$$.value = 0;
		}
	| undefsym D_SET A
		{
			if($1.sym->literal!=NULL) free($1.sym->literal);
			$1.sym->literal=(char *)malloc(2);
			strcpy($1.sym->literal, "A");
			$1.sym->type = SET;
			$1.sym->value = 0;
			$$.value = 0;
		}
	| undefsym D_SET expr
		{
			if($1.sym->literal!=NULL)
			{
				free($1.sym->literal);
				$1.sym->literal=NULL;
			}
			$1.sym->type = SET;
			$1.sym->value = $3.val.v;
			$$.value = 0;
		}
	| undefsym D_SEG D_CODE
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=CSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_XDATA
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=XSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_XDATA OVERL
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=XSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE|OVERLAYABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_DATA
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=DSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_DATA OVERL
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=DSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE|OVERLAYABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_IDATA
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT");  YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=ISEG;
			$1.sym->flags=SEGMENT|RELOCATABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_IDATA OVERL
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=ISEG;
			$1.sym->flags=SEGMENT|RELOCATABLE|OVERLAYABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_BIT
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=BSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| undefsym D_SEG D_BIT OVERL
		{
			if(ObjectFile==0) { NoObjectFileError("SEGMENT"); YYABORT; }

			if(strlen(MapFile)==0) $1.sym->value=0;
			$1.sym->segment=NULL;
			$1.sym->type=BSEG;
			$1.sym->flags=SEGMENT|RELOCATABLE|OVERLAYABLE;
			$1.sym->size=0;
			$$.value = 0;
		}
	| D_RSEG SYMBOL
		{
			if(ObjectFile==0) { NoObjectFileError("RSEG"); YYABORT; }

			if($2.sym->type==UNDEF)
			{
				error("Undefined segment: %s", $2.sym->name);
			}
			else
			{
				current_seg=$2.sym;
				if((pass2)&&(current_seg->type==CSEG))
				{
					fprintf(fobj, "</CODE>\n\n");
					fprintf(fobj, "<CODE %s>\n", current_seg->name);
				}
				if(pass2) emitaddr(current_seg->value); // Warning: this must be erased!
				$$.value = 0;
			}
		}
	| PUBLIC publist
		{
			if(ObjectFile==0) { NoObjectFileError("PUBLIC"); YYABORT; }
			$$.value = 0;
		}
	| EXTERN D_CODE '(' extlist_code ')'
		{
			if(ObjectFile==0) { NoObjectFileError("EXTERN"); YYABORT; }
			$$.value = 0;
		}
	| EXTERN D_DATA '(' extlist_data ')'
		{
			if(ObjectFile==0) { NoObjectFileError("EXTERN"); YYABORT; }
			$$.value = 0;
		}
	| EXTERN D_XDATA '(' extlist_xdata ')'
		{
			if(ObjectFile==0) { NoObjectFileError("EXTERN"); YYABORT; }
			$$.value = 0;
		}
	| EXTERN D_IDATA '(' extlist_idata ')'
		{
			if(ObjectFile==0) { NoObjectFileError("EXTERN"); YYABORT; }
			$$.value = 0;
		}
	| EXTERN D_BIT '(' extlist_bit ')'
		{
			if(ObjectFile==0) { NoObjectFileError("EXTERN"); YYABORT; }
			$$.value = 0;
		}
	/*| D_END // Since this one could be the last one in the file, do not require \n.  See below...
		{
			$$.value = 0;
		}*/
;

/* CONDITIONALS: */

conditional:

	IF data16
		{
			IF_Level++;
			if(IF_Level>sizeof(IF_Flag))
			{
				error("Too many nested IFs");
			}
			if(pass1)
			{
				IF_Flag[IF_Level]=$2.val.v?1:0;
				IF_Value[IF_Count]=$2.val.v?1:0;
				IF_Count++;
				if(IF_Count>sizeof(IF_Value))
				{
					error("Too many IFs");
				}
			}
			else // use the value saved in pass 1
			{
				IF_Flag[IF_Level]=IF_Value[IF_Count];
				IF_Count++;
			}
			$$.value = 0;
		}

	| IFDEF
		{
			IF_Level++;
			if(IF_Level>sizeof(IF_Flag))
			{
				error("Too many nested IFs");
			}
			if(pass1)
			{
				IF_Flag[IF_Level]=( $1.val.v == 1 )?1:0;
				IF_Value[IF_Count]=( $1.val.v == 1 )?1:0;
				IF_Count++;
				if(IF_Count>sizeof(IF_Value))
				{
					error("Too many IFDEFs");
				}
			}
			else // use the value saved in pass 1
			{
				IF_Flag[IF_Level]=IF_Value[IF_Count];
				IF_Count++;
			}
			$$.value = 0;
		}

	| IFNDEF
		{
			IF_Level++;
			if(IF_Level>sizeof(IF_Flag))
			{
				error("Too many nested IFs");
			}
			if(pass1)
			{
				IF_Flag[IF_Level]=( $1.val.v == 1 )?0:1;
				IF_Value[IF_Count]=( $1.val.v == 1 )?0:1;
				IF_Count++;
				if(IF_Count>sizeof(IF_Value))
				{
					error("Too many IFNDEFs");
				}
			}
			else // use the value saved in pass 1
			{
				IF_Flag[IF_Level]=IF_Value[IF_Count];
				IF_Count++;
			}
			$$.value = 0;
		}
	| ELSE
		{
			if(IF_Level==0)
			{
				error("foound ELSE without matching IF");
			}
			else
			{
				IF_Flag[IF_Level] = (IF_Flag[IF_Level]==0?1:0);
			}
			$$.value = 0;
		}
	| ENDIF
		{
			if(IF_Level==0)
			{
				error("found ENDIF without matching IF");
			}
			else
			{
				IF_Level--;
			}
			$$.value = 0;
		}
	| D_END
	{
		force_EOF=1;
		$$.value = 0;
	}
;

defexpr:
	expr
		{
			if( $1.val.d == 0 )
				error("Expression is undefined in pass 1");
			if( !(isbit16($1.val.v)) )
				error("Value greater than 16-bits");
			$$.value = $1.val.v;
		}
;

undefsym:
	SYMBOL
		{
			if( $1.sym->type != SET )
			{ 
				// Only 'set' symbols are redefinable, all other generate an error
				if( $1.sym->type != UNDEF && pass1)
					if(strncmp($1.sym->name, "C?", 2)) // Ignore badly generated C51 Debug symbols
						error("Attempt to redefine symbol: %s",$1.sym->name);
			}
			$1.sym->segment=current_seg;
			$$.sym = $1.sym;
		}
;

publist:
	SYMBOL
	{
		if( $1.sym->type == UNDEF && pass2)
			error("Undefined symbol: %s",$1.sym->name);
		//Mark the symbol as public
		$1.sym->flags=PUBLIC_SYM;
	}
	| publist ',' SYMBOL
	{
		if( $3.sym->type == UNDEF && pass2)
			error("Undefined symbol: %s",$3.sym->name);
		//Mark the symbol as public
		$3.sym->flags=PUBLIC_SYM;
	}
;

extlist_code:
	undefsym
	{
		$1.sym->type = CSEG;
		$1.sym->value = 0;
		$1.sym->size = 1;
		$1.sym->flags=EXTERN_SYM;
	}
	| undefsym ':' expr
	{
		$1.sym->type = CSEG;
		$1.sym->value = 0;
		$1.sym->size = $3.value;
		$1.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym
	{
		$3.sym->type = CSEG;
		$3.sym->value = 0;
		$3.sym->size = 1;
		$3.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym ':' expr
	{
		$3.sym->type = CSEG;
		$3.sym->value = 0;
		$3.sym->size = $5.value;
		$3.sym->flags=EXTERN_SYM;
	}
;

extlist_bit:
	undefsym
	{
		$1.sym->type = BSEG;
		$1.sym->value = 0;
		$1.sym->size = 1;
		$1.sym->flags=EXTERN_SYM;
	}
	| undefsym ':' expr
	{
		$1.sym->type = BSEG;
		$1.sym->value = 0;
		$1.sym->size = $3.value;
		$1.sym->flags=EXTERN_SYM;
	}
	| extlist_bit ',' undefsym
	{
		$3.sym->type = BSEG;
		$3.sym->value = 0;
		$3.sym->size = 1;
		$3.sym->flags=EXTERN_SYM;
	}
	| extlist_bit ',' undefsym ':' expr
	{
		$3.sym->type = BSEG;
		$3.sym->value = 0;
		$3.sym->size = $5.value;
		$3.sym->flags=EXTERN_SYM;
	}

;

extlist_data:
	undefsym
	{
		$1.sym->type = DSEG;
		$1.sym->value = 0;
		$1.sym->size = 1;
		$1.sym->flags=EXTERN_SYM;
	}
	| undefsym ':' expr
	{
		$1.sym->type = DSEG;
		$1.sym->value = 0;
		$1.sym->size = $3.value;
		$1.sym->flags=EXTERN_SYM;
	}
	| extlist_data ',' undefsym
	{
		$3.sym->type = DSEG;
		$3.sym->value = 0;
		$3.sym->size = 1;
		$3.sym->flags=EXTERN_SYM;
	}
	| extlist_data ',' undefsym ':' expr
	{
		$3.sym->type = DSEG;
		$3.sym->value = 0;
		$3.sym->size = $5.value;
		$3.sym->flags=EXTERN_SYM;
	}
;

extlist_idata:
	undefsym
	{
		$1.sym->type = ISEG;
		$1.sym->value = 0;
		$1.sym->size = 1;
		$1.sym->flags=EXTERN_SYM;
	}
	| undefsym ':' expr
	{
		$1.sym->type = ISEG;
		$1.sym->value = 0;
		$1.sym->size = $3.value;
		$1.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym
	{
		$3.sym->type = ISEG;
		$3.sym->value = 0;
		$3.sym->size = 1;
		$3.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym ':' expr
	{
		$3.sym->type = ISEG;
		$3.sym->value = 0;
		$3.sym->size = $5.value;
		$3.sym->flags=EXTERN_SYM;
	}
;

extlist_xdata:
	undefsym
	{
		$1.sym->type = XSEG;
		$1.sym->value = 0;
		$1.sym->size = 1;
		$1.sym->flags=EXTERN_SYM;
	}
	| undefsym ':' expr
	{
		$1.sym->type = XSEG;
		$1.sym->value = 0;
		$1.sym->size = $3.value;
		$1.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym
	{
		$3.sym->type = XSEG;
		$3.sym->value = 0;
		$3.sym->size = 1;
		$3.sym->flags=EXTERN_SYM;
	}
	| extlist_code ',' undefsym ':' expr
	{
		$3.sym->type = XSEG;
		$3.sym->value = 0;
		$3.sym->size = $5.value;
		$3.sym->flags=EXTERN_SYM;
	}
;

blist:
	blist ',' data8
		{
			if( pass2 )
			{
				if(ObjectFile)
				{
					if($3.val.s==1)
					{
						fprintf(fobj, "%s\n", $3.val.rel);
					}
					else
					{
						objbyte($3.value);
					}
				}
				genbyte($3.value);
			}
			$$.value = $1.value + 1;
		}
	| blist ',' STRING
		{
			$$.value = $1.value + strlen($3.str);
			if( pass2 )
			{
				genstr($3.str);
				if(ObjectFile) objstr($3.str);
			}
			free($3.str);
		}
	| data8
		{
			if( pass2 )
			{
				if(ObjectFile)
				{
					if($1.val.s==1)
					{
						fprintf(fobj, "%s\n", $1.val.rel);
					}
					else
					{
						objbyte($1.value);
					}
				}
				genbyte($1.value);
			}
			$$.value = 1;
		}
	| STRING
		{
			$$.value = strlen($1.str);
			if( pass2 )
			{
				genstr($1.str);
				if(ObjectFile) objstr($1.str);
			}
			free($1.str);
		}
;

wlist:
	wlist ',' data16
		{
			if( pass2 )
			{
				if(ObjectFile)
				{
					if($3.val.s==1)
					{
						fprintf(fobj, "%s\n", $3.val.rel);
					}
					else
					{
						objword($3.value);
					}
				}
				genword($3.value);
			}
			$$.value = $1.value + 2;
		}
	| data16
		{
			if( pass2 )
			{
				if(ObjectFile)
				{
					if($1.val.s==1)
					{
						fprintf(fobj, "%s\n", $1.val.rel);
					}
					else
					{
						objword($1.value);
					}
				}
				genword($1.value);
			}
			$$.value = 2;
		}
;

/* EXPRESSIONS: */

expr:
	'$'
		{
			$$.val.v = current_seg->value;
			$$.val.d = 1;
			if(current_seg->flags&RELOCATABLE)
			{
				// There is a problem with relocatable expressions and the 'current program counter'
				// operator '$'.  This is because we don't know for what instruction the '$' was
				// generated, so when locating code we only know the position where the operator apperead
				// and that is what the linker is going to use.
				strcat(symexp,"$;");
				$$.val.s = 1;
				$$.val.k = 1;
			}
		}
	| '(' expr ')'
		{
			$$.val.s = $2.val.s;
			$$.val.v = $2.val.v;
			$$.val.d = $2.val.d;
			$$.val.k = $2.val.k;
		}
	| '-' expr %prec '*'
		{
			strcat(symexp,"0xFFFF;*;");
			$$.val.s = $2.val.s;
			$$.val.v = -$2.val.v;
			$$.val.d = $2.val.d;
			$$.val.k = $2.val.k;
		}
	| HIGH expr
		{
			strcat(symexp,"0x0008;>>;");
			$$.val.s = $2.val.s;
			$$.val.v = (($2.val.v / 0x100) & 0xff);
			$$.val.d = $2.val.d;
			$$.val.k = $2.val.k;
		}
	| LOW expr
		{
			strcat(symexp,"0x00FF;&;");
			$$.val.s = $2.val.s;
			$$.val.v = (($2.val.v % 0x100) & 0xff);
			$$.val.d = $2.val.d;
			$$.val.k = $2.val.k;
		}
	| NOT expr //bitwise NOT
		{
			strcat(symexp,"~;");
			$$.val.s = $2.val.s;
			$$.val.v = (~$2.val.v & 0xffff);
			$$.val.d = $2.val.d;
			$$.val.k = $2.val.k;
		}
	| expr '|' expr
		{
			strcat(symexp,"|;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v | $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr OR expr
		{
			strcat(symexp,"|;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v | $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '&' expr
		{
			strcat(symexp,"&;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v & $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr AND expr
		{
			strcat(symexp,"&;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v & $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '^' expr
		{
			strcat(symexp,"^;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v ^ $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr XOR expr
		{
			strcat(symexp,"^;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v ^ $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '*' expr
		{
			strcat(symexp,"*;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v * $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '/' expr
		{
			strcat(symexp,"/;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.k = $1.val.k | $3.val.k;
			if(($3.val.v!=0)&&($$.val.s==0))
			{
				$$.val.v = $1.val.v / $3.val.v;
				$$.val.d = $1.val.d && $3.val.d;
			}
			else if($$.val.s==1)
			{
				$$.val.v = 1; // Relocatable symbolic expression: doesn't matter what I return!
				$$.val.d = $1.val.d && $3.val.d;
			}
			else
			{
				error("Divide by zero");
			}
		}
	| expr '%' expr
		{
			strcat(symexp,"%;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.k = $1.val.k | $3.val.k;
			if(($3.val.v!=0)&&($$.val.s==0))
			{
				$$.val.v = $1.val.v % $3.val.v;
				$$.val.d = $1.val.d && $3.val.d;
			}
			else if($$.val.s==1)
			{
				$$.val.v = 1; // Relocatable symbolic expression: doesn't matter what I return!
				$$.val.d = $1.val.d && $3.val.d;
			}
			else
			{
				error("Divide by zero");
			}
		}
	| expr '-' expr
		{
			strcat(symexp,"-;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v - $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '+' expr
		{
			strcat(symexp,"+;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v + $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '>' '>' expr
		{
			strcat(symexp,">>;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = $1.val.v >> $4.val.v;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr SHR expr
		{
			strcat(symexp,">>;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v >> $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '<' '<' expr
		{
			strcat(symexp,"<<;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = $1.val.v << $4.val.v;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr SHL expr
		{
			strcat(symexp,"<<;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = $1.val.v << $3.val.v;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '=' expr
		{
			strcat(symexp,"=;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v == $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr C_EQ expr
		{
			strcat(symexp,"=;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v == $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '=' '=' expr
		{
			strcat(symexp,"=;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = ($1.val.v == $4.val.v)?1:0;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr '!' '=' expr
		{
			strcat(symexp,"!=;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = ($1.val.v == $4.val.v)?0:1;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr '<' '>' expr
		{
			strcat(symexp,"!=;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = ($1.val.v == $4.val.v)?0:1;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr C_NE expr
		{
			strcat(symexp,"!=;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v == $3.val.v)?0:1;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '<' expr
		{
			strcat(symexp,"<;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v < $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr C_LT expr
		{
			strcat(symexp,"<;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v < $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '<' '=' expr
		{
			strcat(symexp,"<=;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = ($1.val.v <= $4.val.v)?1:0;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr C_LE expr
		{
			strcat(symexp,"<=;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v <= $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '>' expr
		{
			strcat(symexp,">;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v > $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr C_GT expr
		{
			strcat(symexp,">;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v > $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| expr '>' '=' expr
		{
			strcat(symexp,">=;");
			$$.val.s = $1.val.s | $4.val.s;
			$$.val.v = ($1.val.v >= $4.val.v)?1:0;
			$$.val.d = $1.val.d && $4.val.d;
			$$.val.k = $1.val.k | $4.val.k;
		}
	| expr C_GE expr
		{
			strcat(symexp,">=;");
			$$.val.s = $1.val.s | $3.val.s;
			$$.val.v = ($1.val.v >= $3.val.v)?1:0;
			$$.val.d = $1.val.d && $3.val.d;
			$$.val.k = $1.val.k | $3.val.k;
		}
	| SYMBOL
		{
			$$.val.k = kmask[$1.sym->type&0xf] | 0x01; // 0x01: means value comes from symbol.

			if( pass1 )
			{
				$$.val.v = $1.sym->value;
				$$.val.d = ($1.sym->type != UNDEF);
			}
			else
			{
				if( $1.sym->type == UNDEF )
				{
					error("Undefined symbol %s", $1.sym->name);
					$$.val.v = 0;
				}
				$$.val.d = 1;
			}
			
			if( $1.sym->type != UNDEF )
			{
				if($1.sym->segment!=NULL) // Does this properly takes care of $1.sym->segment==NULL?  See below.
				{
					if ($1.sym->segment->flags&RELOCATABLE) 
					{
						strcat(symexp,$1.sym->name);
						strcat(symexp,";");
						$$.val.v = $1.val.v;
						if( (strlen(MapFile)>0) && pass2 )
						{
							$$.val.v = $1.sym->value;
							$$.val.s = 0; // If loading symbols from mapfile, no symbolic expressions
						}
						else
						{
							$$.val.s = 1; // Relocatable symbolic expression
						}
					}
					else
					{
						sprintf(tmpexp,"0x%04X;",$1.sym->value);
						strcat(symexp,tmpexp);
						$$.val.v = $1.sym->value;
						$$.val.s = 0;
					}
				}
				else // For symbols without a parent segment (EQUs, on the fly externs, etc.)
				{
					if ($1.sym->flags&EXTERN_SYM)
					{
						strcat(symexp,$1.sym->name);
						strcat(symexp,";");
						$$.val.v = 0;
						if( (strlen(MapFile)>0) && pass2 )
						{
							$$.val.v = $1.sym->value;
							$$.val.s = 0; // If loading symbols from mapfile, no symbolic expressions
						}
						else
						{
							$$.val.s = 1; // Relocatable symbolic expression
						}
					}
					else
					{
						sprintf(tmpexp,"0x%04X;",$1.sym->value);
						strcat(symexp,tmpexp);
						$$.val.v = $1.sym->value;
						$$.val.s = 0;
					}
				}
			}

		}
	| VALUE
		{
			sprintf(tmpexp,"0x%04X;",$1.val.v);
			strcat(symexp,tmpexp);
			$$.val.k = 0;
			$$.val.s = 0;
			$$.val.v = $1.val.v; $$.val.d=1;
		}
;


/* INSTRUCTIONS: */

instr:
	NOP
		{ $$.value = makeop($1.op,NULL,0); }
	| ACALL addr11
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| AJMP addr11
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| ADD two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| ADDC two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| SUBB two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| XRL two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| XRL two_op2
		{ $$.value = makeop($1.op,&$2.mode,4); }
	| ANL two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| ANL two_op2
		{ $$.value = makeop($1.op,&$2.mode,4); }
	| ANL two_op3
		{ $$.value = makeop($1.op,&$2.mode,6); }
	| ORL two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| ORL two_op2
		{ $$.value = makeop($1.op,&$2.mode,4); }
	| ORL two_op3
		{ $$.value = makeop($1.op,&$2.mode,6); }
	| XCH two_op1
		{ if( get_md($2.mode) == 3 )
			error("Immediate mode is invalid");
		  $$.value = makeop($1.op,&$2.mode,0); }
	| INC single_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| INC DPTR
		{ $$.value = makeop($1.op,NULL,4); }
	| DEC single_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| DA A
		{ $$.value = makeop($1.op,NULL,0); }
	| DIV AB
		{ $$.value = makeop($1.op,NULL,0); }
	| JMP '@' A '+' DPTR
		{ $$.value = makeop($1.op,NULL,0); }
	| JMP '@' DPTR '+' A
		{ $$.value = makeop($1.op,NULL,0); }
	| JMP addr16
	{
		long offset;
		unsigned long address;
		static unsigned char ajmp[]= { 0x01 };
		static unsigned char ljmp[]= { 0x02 };
		static unsigned char sjmp[]= { 0x80 };
		static struct opcode opjmp[] = {
			{"ajmp",    AJMP,   ajmp  },
			{"ljmp",    LJMP,   ljmp  },
			{"sjmp",    SJMP,   sjmp  }
		};
		//if(ObjectFile) error("'JMP label' instruction is not allowed when compiling to an object file");
		if(ObjectFile==0)
		{
			address=$2.mode.byte2+($2.mode.byte1*0x100);
			offset = address - (current_seg->value+2);
			//if(pass2) printf("address=%04XH, current_seg->value+2=%04XH, offset is %d\n", address, current_seg->value+2, offset);
			if( (offset <= 127) && (offset >= -128) && ($2.mode.str1[0]==0) && (address<=current_seg->value))
			{  // SJMP
				$2.mode.mode=0;
				$2.mode.orval=0;
				$2.mode.size=1;
				$2.mode.byte1=offset;
				$$.value = makeop(&opjmp[2],&$2.mode,0);
			}
			else if( ( (address&0xF800) == ((current_seg->value+2)&0xF800) ) && ($2.mode.str1[0]==0) && (address<=current_seg->value) ) // Same page?
			{  // AJMP
				$2.mode.mode=0;
				$2.mode.orval= (unsigned char) ((address&0x0700)>>3) ;
				$2.mode.size=1;
				$2.mode.byte1=address&0x00FF;
				$$.value = makeop(&opjmp[0],&$2.mode,0);
			}
			else
			{  // LJMP
				$$.value = makeop(&opjmp[1],&$2.mode,0);
			}
		}
		else // ObjectFile==1
		{  // LJMP
			$$.value = makeop(&opjmp[1],&$2.mode,0);
		}
	}
	| CALL addr16
	{
		long offset;
		unsigned long address;
		static unsigned char acall[]= { 0x11 };
		static unsigned char lcall[]= { 0x12 };
		static struct opcode opcall[] = {
			{"acall",    ACALL,   acall  },
			{"lcall",    LCALL,   lcall  }
		};
		//if(ObjectFile) error("'CALL label' instruction is not allowed when compiling to an object file");
		if(ObjectFile==0)
		{
			address=$2.mode.byte2+($2.mode.byte1*0x100);
			offset = address - (current_seg->value+2);
			//if(pass2) printf("address=%04XH, current_seg->value+2=%04XH, offset is %d\n", address, current_seg->value+2, offset);
			if( ( (address&0xF800) == ((current_seg->value+2)&0xF800) ) && ($2.mode.str1[0]==0) && (address<=current_seg->value) ) // Same page?
			{  // ACALL
				$2.mode.mode=0;
				$2.mode.orval= (unsigned char) ((address&0x0700)>>3) ;
				$2.mode.size=1;
				$2.mode.byte1=address&0x00FF;
				$$.value = makeop(&opcall[0],&$2.mode,0);
			}
			else
			{  // LCALL
				$$.value = makeop(&opcall[1],&$2.mode,0);
			}
		}
		else // ObjectFile==1
		{  // LCALL
			$$.value = makeop(&opcall[1],&$2.mode,0);
		}
	}
	| MUL AB
		{ $$.value = makeop($1.op,NULL,0); }
	| RET
		{ $$.value = makeop($1.op,NULL,0); }
	| RETI
		{ $$.value = makeop($1.op,NULL,0); }
	| RL A
		{ $$.value = makeop($1.op,NULL,0); }
	| RLC A
		{ $$.value = makeop($1.op,NULL,0); }
	| RR A
		{ $$.value = makeop($1.op,NULL,0); }
	| RRC A
		{ $$.value = makeop($1.op,NULL,0); }
	| SWAP A
		{ $$.value = makeop($1.op,NULL,0); }
	| XCHD two_op1
		{
			if( get_md($2.mode) != 2 )
				error("Invalid addressing mode");
			$$.value = makeop($1.op,&$2.mode,-2);
		}
	| CLR single_op2
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| CPL single_op2
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| SETB single_op2
		{
			if( get_md($2.mode) == 0 ) error("Invalid addressing mode");
			$$.value = makeop($1.op,&$2.mode,-1);
		}
	| PUSH data8
		{
			struct mode tmp;
			set_md(tmp,0);
			set_ov(tmp,0);
			set_sz(tmp,1);
			set_b1(tmp,$2.value);
			set_str1(&tmp, &$2);
			$$.value = makeop($1.op,&tmp,0);
		}
	| POP data8
		{
		   struct mode tmp;
			set_md(tmp,0);
			set_ov(tmp,0);
			set_sz(tmp,1);
			set_b1(tmp,$2.value);
			set_str1(&tmp, &$2);
			$$.value = makeop($1.op,&tmp,0);
		}
	| LJMP addr16
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| LCALL addr16
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JC relative
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JNC relative
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JNZ relative
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JZ relative
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| SJMP relative
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| CJNE three_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JB two_op4
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JNB two_op4
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| JBC two_op4
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| DJNZ two_op5
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| MOV two_op1
		{ $$.value = makeop($1.op,&$2.mode,0); }
	| MOV two_op2
		{ $$.value = makeop($1.op,&$2.mode,4); }
	| MOV two_op6
		{ $$.value = makeop($1.op,&$2.mode,6); }
	| MOVC A ',' '@' A '+' DPTR
		{ $$.value = makeop($1.op,NULL,0); }
	| MOVC A ',' '@' DPTR '+' A
		{ $$.value = makeop($1.op,NULL,0); }
	| MOVC A ',' '@' A '+' PC
		{ $$.value = makeop($1.op,NULL,1); }
	| MOVC A ',' '@' PC '+' A
		{ $$.value = makeop($1.op,NULL,1); }
	| MOVX A ',' '@' regi
		{ $$.value = makeop($1.op,NULL,$5.value); }
	| MOVX A ',' '@' DPTR
		{ $$.value = makeop($1.op,NULL,2); }
	| MOVX '@' regi ',' A
		{ $$.value = makeop($1.op,NULL,$3.value+3); }
	| MOVX '@' DPTR ',' A
		{ $$.value = makeop($1.op,NULL,5); }
;


/* ADDRESSING MODES: */

two_op1:
	A ',' reg
		{
			set_md($$.mode,0);
			set_ov($$.mode, $3.value);
			set_sz($$.mode, 0);
		}
	| A ',' data8
		{
			if(($3.val.k==0) && pass1 && hash_warning_enable)
			{
				warning("maybe '#' is missing with constant expression.");
			}
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode, &$3);
		}
	| A ',' '@' regi
		{
			set_md($$.mode,2);
			set_ov($$.mode,$4.value);
			set_sz($$.mode,0);
		}
	| A ',' '#' data8
		{
			set_md($$.mode,3);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode, &$4);
		}
;

two_op2:
	data8 ',' A
		{
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode, &$1);
		}
	| data8 ',' '#' data8
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
			set_b2($$.mode,$4.value);
			set_str2(&$$.mode,&$4);
		}
;

two_op3:
	C ',' bit
		{
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
		}
	| C ',' '/' bit
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
		}
	| C ',' '!' bit  //This is not valid syntax!
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
		}
;

two_op4:
	bit ',' rel
		{
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
			set_b2($$.mode,$3.value);
			set_str2(&$$.mode,&$3);
		}
;

two_op5:
	reg ',' rel2
		{
			set_md($$.mode,0);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
		}
	| data8 ',' rel
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
			set_b2($$.mode,$3.value);
			set_str2(&$$.mode,&$3);
		}
;

two_op6:
	reg ',' A
		{
			set_md($$.mode,0);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,0);
		}
	| reg ',' data8
		{
			if(($3.val.k==0) && pass1 && hash_warning_enable)
			{
				warning("maybe '#' is missing with constant expression.");
			}
			set_md($$.mode,1);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
		}
	| reg ',' '#' data8
		{
			set_md($$.mode,2);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
		}
	| data8 ',' reg
		{
			set_md($$.mode,3);
			set_ov($$.mode,$3.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
		}
	| data8 ',' data8
		{
			if(($3.val.k==0) && pass1 && hash_warning_enable)
			{
				warning("maybe '#' is missing with constant expression.");
			}
			set_md($$.mode,4);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
			set_b2($$.mode,$1.value);
			set_str2(&$$.mode,&$1);
		}
	| data8 ',' '@' regi
		{
			set_md($$.mode,5);
			set_ov($$.mode,$4.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
		}
	| '@' regi ',' A
		{
			set_md($$.mode,6);
			set_ov($$.mode,$2.value);
			set_sz($$.mode,0);
		}
	| '@' regi ',' data8
		{
			if(($4.val.k==0) && pass1 && hash_warning_enable)
			{
				warning("maybe '#' is missing with constant expression.");
			}
			set_md($$.mode,7);
			set_ov($$.mode,$2.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
		}
	| '@' regi ',' '#' data8
		{
			set_md($$.mode,8);
			set_ov($$.mode,$2.value);
			set_sz($$.mode,1);
			set_b1($$.mode,$5.value);
			set_str1(&$$.mode,&$5);
		}
	| DPTR ',' '#' data16
		{
			set_md($$.mode,9);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode, ($4.value & 0xff00) >> 8 );
			set_str1(&$$.mode, &$4);
			set_b2($$.mode, ($4.value & 0x00ff) );
			// There is a problem with the data16 production below.  For now, fix it here...
			if($4.val.s==1) // if 'data16' is a symbolic expression, the second string is blank.
			{
				$$.mode.str2[0]=' ';
				$$.mode.str2[1]=0;
			}
			else // if 'data16' is numeric, we need to print the lower 8-bits of the value
			{
				$$.mode.str2[0]=0;
			}
		}
	| C ',' bit
		{
			set_md($$.mode,10);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
		}

	/* The following two productions cannot be represented by:
	 *
	 *	bit ',' C
	 *
	 * Because yacc gives tons of reduce/reduce errors if
 	 * that is attempted. */

	/*| bit ',' C
		{
			set_md($$.mode,11);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
		}*/
	| data8 ',' C
		{
			set_md($$.mode,11);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
		}
	| data8 BITPOS ',' C
		{
			if( pass2 )
			{
				if($1.val.s==1)
				{
					sprintf($$.mode.str1, "bit(%s%04X;.;)", $1.val.rel, $2.value);
					$$.mode.str2[0]=' ';
					$$.mode.str2[1]=0;	
				}
				else
				{
					if( !isbit8($1.value) )
						if(ObjectFile==0) error("Bit address exceeds 8-bits");
					if( isbmram($1.value) )
						set_b1($$.mode, ($1.value-0x20)*8+ $2.value );
					else if( isbmsfr($1.value) )
						set_b1($$.mode, $1.value + $2.value );
					else
						if(ObjectFile==0) error("Invalid bit addressable RAM location");
				}
			}
			set_md($$.mode,11);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
		}
;

single_op1:
	A
		{
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,0);
		}
	| reg
		{
			set_md($$.mode,1);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,0);
		}
	| data8
		{
			set_md($$.mode,2);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
		}
	| '@' regi
		{
			set_md($$.mode,3);
			set_ov($$.mode,$2.value);
			set_sz($$.mode,0);
		}
;

single_op2:
	A
		{
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,0);
		}
	| C
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,0);
		}
	| bit
		{
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.mode.str1, "%", symexp);
				$$.mode.str2[0]=' ';
				$$.mode.str2[1]=0;
				$$.val.s=1;
			}
			set_md($$.mode,2);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			set_b1($$.mode,$1.value);
			set_str1(&$$.mode,&$1);
		}
;

three_op1:
	A ',' data8 ',' rel
		{
			if(($3.val.k==0) && pass1 && hash_warning_enable)
			{
				warning("maybe '#' is missing with constant expression.");
			}
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$3.value);
			set_str1(&$$.mode,&$3);
			set_b2($$.mode,$5.value);
			set_str2(&$$.mode,&$5);
		}
	| A ',' '#' data8 ',' rel
		{
			set_md($$.mode,1);
			set_ov($$.mode,0);
			set_sz($$.mode,2);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
			set_b2($$.mode,$6.value);
			set_str2(&$$.mode,&$6);
		}
	| reg ',' '#' data8 ',' rel
		{
			set_md($$.mode,2);
			set_ov($$.mode,$1.value);
			set_sz($$.mode,2);
			set_b1($$.mode,$4.value);
			set_str1(&$$.mode,&$4);
			set_b2($$.mode,$6.value);
			set_str2(&$$.mode,&$6);
		}
	| '@' regi ',' '#' data8 ',' rel
		{
			set_md($$.mode,3);
			set_ov($$.mode,$2.value);
			set_sz($$.mode,2);
			set_b1($$.mode,$5.value);
			set_str1(&$$.mode,&$5);
			set_b2($$.mode,$7.value);
			set_str2(&$$.mode,&$7);
		}
;

rel:
	expr
		{
			long offset;

			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.val.rel, "rel3(%s)", symexp);
				$$.val.s=1;
			}
			else if( pass2 )
			{
				offset = $1.val.v - (current_seg->value+3);
				if( offset > 127 || offset < -128 )
				   error("Relative offset exceeds -128 / +127");
				$$.value = offset;
				$$.val.s=0;
			}
			symexp[0]=0;
		}
;

/* This production differs from the above, by 1 number! */
rel2:
	expr
		{
			long offset;
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.val.rel, "rel2(%s)", symexp);
				$$.val.s=1;
			}
			else if( pass2 )
			{
				offset = $1.val.v - (current_seg->value+2); /* different! */
				if( offset > 127 || offset < -128 )
				   error("Relative offset exceeds -128 / +127");
				$$.value = offset;
				$$.val.s=0;
			}
			symexp[0]=0;
		}
;

bit:
	expr BITPOS
		{
			static unsigned char sn[0x100]="";
			if( pass2 )
			{
				if($1.val.s==1)
				{
					sprintf($$.val.rel, "bit(%s%04X;.;)", symexp, $2.value);
					$$.val.s=1;
				}
				else
				{
					if( !isbit8($1.value) )
						if(ObjectFile==0) error("Bit address exceeds 8-bits");
					if( isbmram($1.value) )
						$$.value = ($1.value-0x20)*8+$2.value;
					else if( isbmsfr($1.value) )
						$$.value = $1.value + $2.value;
					else
						if(ObjectFile==0) error("Invalid bit addressable RAM location");
					$$.val.s=0;
				}
			}
			symexp[0]=0;
		}
	| expr
		{
			//if((pass2)&&($1.val.s==1)&&isbmsfr($1.value))
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.val.rel, "bit(%s)", symexp);
				$$.val.s=1;
			}
			else if( pass2 )
			{
				if( !isbit8($1.value) && (ObjectFile==0) )	error("Bit address exceeds 8-bits");
				$$.value = $1.value;
				$$.val.s=0;

				if(ObjectFile==0)
				{
					if ( (($1.val.k&kmask[BSEG])==kmask[BSEG]) || ($1.val.k==0) )
					{
						// The address of the bit is good
					}
					else
					{
						error("invalid bit address.");
					}
				}
			}
			symexp[0]=0;
		}
;

/*
bitv:
	SYMBOL
		{
			if( $1.sym->type == UNDEF && pass2 )
				error("Symbol %s undefined",$1.sym->name);
			$$.value = $1.sym->value;
		}
	| VALUE
		{ $$.value = $1.value; }
;*/

reg:
	  R0 { $$.value = 0; $$.val.k=1; }
	| R1 { $$.value = 1; $$.val.k=1; }
	| R2 { $$.value = 2; $$.val.k=1; }
	| R3 { $$.value = 3; $$.val.k=1; }
	| R4 { $$.value = 4; $$.val.k=1; }
	| R5 { $$.value = 5; $$.val.k=1; }
	| R6 { $$.value = 6; $$.val.k=1; }
	| R7 { $$.value = 7; $$.val.k=1; }
;

areg:
	  AR0 { $$.value = 0; $$.val.k=1; }
	| AR1 { $$.value = 1; $$.val.k=1; }
	| AR2 { $$.value = 2; $$.val.k=1; }
	| AR3 { $$.value = 3; $$.val.k=1; }
	| AR4 { $$.value = 4; $$.val.k=1; }
	| AR5 { $$.value = 5; $$.val.k=1; }
	| AR6 { $$.value = 6; $$.val.k=1; }
	| AR7 { $$.value = 7; $$.val.k=1; }
;

regi:
	reg
		{
			if($1.val.v<2) $$.value = $1.val.v;
			else
			{
				$$.value = 0;
				error("Invalid indirect register: @r%d", $1.val.v);
			}
		}
;

data8:
	expr
		{
			if((pass2)&&($$.val.s==1))
			{
				sprintf($$.val.rel,"data8(%s)", symexp);
				$$.val.s=1;
			}
			else if( pass2 )
			{
				if($$.val.s==0)
				{
					if( !isbit8($1.val.v) && (ObjectFile==0) ) error("Expression greater than 8-bits");
				}
				$$.value = $1.val.v;// & 0xff; //Allow 16-bit data expression in object files.  They are truncated by the linker.
				$$.val.s=0;
			}
			symexp[0]=0;
		}
	
	| areg
		{ $$.value = $1.val.v+(8*RegisterBank); }
;

data16:
	expr
		{
			if((pass2)&&($$.val.s==1))
			{
				sprintf($$.val.rel,"data16(%s)", symexp);
				$$.val.s=1;
			}
			else
			{
				if( pass2 )
				{
					if( !isbit16($1.val.v)  && (ObjectFile==0) ) error("Expression greater than 16-bits");
				}
				$$.value = $1.val.v;
				$$.val.s=0;
			}
			symexp[0]=0;
		}
	| STRING
		{
			if(strlen($1.str)!=2)
			{
				error("Invalid 16-bit string constant");
			}
			else
			{
				$$.value = ($1.str[0]*0x100)+$1.str[1];
			}
			free($1.str);
		}
;

addr11:
	expr
		{
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.mode.str1, "addr11(%s)", symexp);
				$$.mode.str2[0]=' ';
				$$.mode.str2[1]=0;
			}
			else
			{
				if( pass2 )
				{
					if( !isbit16($1.val.v)  )
						error("Address greater than 16-bits");
					if( ((unsigned long)($1.val.v & size11)) != ((current_seg->value+2) & size11) )
						error("Address outside current 2K page");
				}
				set_b1($$.mode,$1.val.v&0x00ff);
			}
			set_md($$.mode,0);
			set_ov($$.mode, ($1.val.v&0x0700)>>3 );
			set_sz($$.mode,1);
			symexp[0]=0;
		}
;

addr16:
	expr
		{
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.mode.str1, "addr16(%s)", symexp);
				$$.mode.str2[0]=' ';
				$$.mode.str2[1]=0;
				$$.val.s=1;
			}
			else
			{
				if( pass2 )
				{
					if( !isbit16($1.val.v)  ) error("Address greater than 16-bits");
				}
				set_b1($$.mode, ($1.val.v & 0xff00 ) >> 8 );
				set_b2($$.mode, ($1.val.v & 0x00ff ) );
				$$.val.v=$1.val.v;
			}
			set_md($$.mode,0);
			set_ov($$.mode, 0 );
			set_sz($$.mode,2);
			symexp[0]=0;
		}
;

relative:  //Check production rel2 above
	expr
		{
			long offset=0;
			if((pass2)&&($1.val.s==1))
			{
				sprintf($$.mode.str1, "rel2(%s)", symexp);
			}
			else
			{
				if( pass2 )
				{
					offset = $1.val.v - (current_seg->value+2);
					if( offset>127 || offset<-128 )
					   error("Relative offset exceeds -128 / +127");
				}
				set_b1($$.mode,offset);
			}
			set_md($$.mode,0);
			set_ov($$.mode,0);
			set_sz($$.mode,1);
			symexp[0]=0;
		}
;

%%

/* ---------------------------------------------------------------------- */

void yyerror(char *s)
{
	error((const char *)"%s near \"%s\"", s, get_last_token());
	//error((const char *)"%s", s);
}

void set_str1(struct mode *m, union ystack *y)
{
	if(y->val.s==1)
	{
		strcpy(m->str1, y->val.rel);
	}
	else
		m->str1[0]=0;
}

void set_str2(struct mode *m, union ystack *y)
{
	if(y->val.s==1)
	{
		strcpy(m->str2, y->val.rel);
	}
	else
		m->str2[0]=0;
}

/* ----------------------------------------------------------------------
 * makeop:
 *	This function makes an opcode based on the instruction symbol table
 *	entry, and an addressing mode structure.
 *	This function is called from both passes, but
 *	only generates code in pass 2.
 *
 *	Resultant opcode bytes are passed to genbyte().
 *
 *	Returns the number of bytes that the instruction
 *	occupies.
 *
 */

int makeop(struct opcode * op, struct mode *m, int add)
{
	register unsigned int newop;

	if( m == NULL )
	{
		if(pass2)
		{
			genbyte(op->bytes[0+add]);
			if(ObjectFile) fprintf(fobj, "%02X\n", op->bytes[0+add]);
		}
		return(1);
	}

	if( pass2 )
	{
		newop = op->bytes[ get_md(*m)+add ] | get_ov(*m);
		genbyte(newop);
		if(ObjectFile) fprintf(fobj, "%02X", newop); 
		
		if( get_sz(*m) > 0 )
		{ 
			genbyte( get_b1(*m) & 0xff );
			if(ObjectFile)
			{
				if (m->str1[0]!=0)
				{
					fprintf(fobj, " %s", m->str1);
				}
				else fprintf(fobj, " %02X", get_b1(*m)&0xff);
			}
		}
		if( get_sz(*m) > 1 )
		{
			genbyte( get_b2(*m) & 0xff );
			if(ObjectFile)
			{
				if (m->str2[0]!=0)
				{
					fprintf(fobj, " %s", m->str2);
				}
				else fprintf(fobj, " %02X", get_b2(*m)&0xff);
			}
		}
	}

	if((ObjectFile)&&(pass2)) fprintf(fobj, "\n");

	return( get_sz(*m)+1 );
}

/* ----------------------------------------------------------------------
 * padline:
 *	This routine returns a new string, which is equivalent to
 *	'line' except that all tabs have been expanded to spaces, and
 *	the total length has been truncated to 0x200 chars.
 */

char *padline(char *line)
{
	static char newline[0x200];
	char *p1;
	int pos=0,nxtpos;

	for(p1=line; pos<sizeof(newline)-1 && *p1; p1++ )
	{
		if( *p1 == '\t' )
		{
			nxtpos = pos+8-pos%8;
			while(pos<sizeof(newline)-1 && pos <= nxtpos)
				newline[pos++] = ' ';
		}
		else if ( ( *p1 != '\n' ) && ( *p1 != '\r' ) )
		{
			newline[pos++]= *p1;
		}
	}
	newline[pos] = '\0';
	return(newline);
}


/* ----------------------------------------------------------------------
 * dumplist:
 *	Outputs the current location counter, bytebuf[] array, and
 *	the string 'txt' to the listing file.
 *	This routine is called for every source line encountered in the
 *	source file. (Only in pass 2, and if listing is turned on).
 *
 */

void dumplist(char *txt, int show)
{
	int i,j,lc;
	int numnl, mylineno, prevnl;

	for(j=0, numnl=0; txt[j]!=0; j++)
	{
		if(txt[j]=='\n') numnl++;
	}
	if(numnl>1)
	{
		mylineno=lineno-numnl+1;
		fprintf(listing, "              %5d   ", mylineno++);
		prevnl=0;
		for(j=0; txt[j]!=0; j++)
		{
			if(txt[j]!='\n')
			{
				if(prevnl)
				{
					fprintf(listing, "\n              %5d   ", mylineno++);
					prevnl=0;
				}
				if(txt[j]!='\r') fprintf(listing, "%c", txt[j]);
			}
			else
			{
				prevnl=1;
			}
		}
		fprintf(listing, "\n");
		return;
	}

	lc=current_seg->value;

	if(show)
	{
		fprintf(listing, "%04X ", lc);
	}
	else
	{
		fprintf(listing, "%04X ", lc);
		//fprintf(listing, "     ");
	}

	for(i=0; i<4; i++ ) {
		if(i<bytecount)
		    fprintf(listing,"%02X",bytebuf[i]);
		else
		    fprintf(listing,"  ");
	}
	fprintf(listing," %5d   %s", lineno, padline(txt));

	for(j=0; i<bytecount; i++, j++)
	{
		if((j%4)==0) fprintf(listing, "\n     ");
		if(i<bytecount) fprintf(listing, "%02X",bytebuf[i]);
	}
	fprintf(listing, "\n");
}

/* ----------------------------------------------------------------------
 * gen* routines:
 *	Place information into the bytebuf[] array, and also
 *	call emitbyte with the byte.
 *
 */

void genbyte(int b)
{
	if( bytecount < sizeof(bytebuf) )
		bytebuf[bytecount++] = b;
	if(ObjectFile==0) emitbyte(b);
}

void genstr(char *s)
{
	while( *s )
		genbyte(*s++);
}

void genword(unsigned long w)
{
	genbyte( (w & 0xff00) >> 8 );
	genbyte( (w & 0x00ff) );
}

void objbyte(int b)
{
	fprintf(fobj, "%02X\n", b);
}

void objstr(char *s)
{
	int j;

	for(j=0; s[j]!=0; j++)
	{
		fprintf(fobj, "%02X", s[j]);
		if((j>0)&&(j%16==0)) fprintf(fobj, "\n");
		else fprintf(fobj, " ");
	}
	fprintf(fobj, "\n");
}

void objword(unsigned long w)
{
	fprintf(fobj, "%02X ",  (w & 0xff00) >> 8 );
	fprintf(fobj, "%02X\n", (w & 0x00ff) );
}

void BadRegUse(char * x)
{
	 error("Invalid use of predefined register '%s'", x);
	 seek_eol();
}

void NoObjectFileError (char * x)
{
	error("Directive '%s' not allowed for absolute compilation.\n", x);
}
