/*  lexer.c

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
#include <ctype.h>
#include <string.h>

#include "a51.h"
#include "parser.h"

int lineno;
extern int IF_Flag[0x100];
extern int IF_Level;
extern char IF_Value[0x1000];
extern int IF_Count;
int macro_count;
extern int force_EOF;

int last_token_len=0;
char last_token[256], prev_token[256];

static char line[0x4000]; //Originally 100, but macros need way more than that
static char *lineptr=line;

int linetest=-1;
extern FILE *fin;

typedef struct
{
   char * str;
   void * prev;
   int cntr;
   int type;
} _literal;

_literal * current_literal=NULL;

void push_literal (char * s, int type)
{
	_literal * new_literal=NULL;

	new_literal=malloc(sizeof(_literal));
    if(new_literal==NULL)
    {
        error("Not enough memory to run in this system\n");
        exit(-1);
    }
	new_literal->str=malloc(strlen(s)+10);
    if(new_literal->str==NULL)
    {
        error("Not enough memory to run in this system\n");
        exit(-1);
    }

	strcpy(new_literal->str, s);
	new_literal->prev=(void *)current_literal;
	new_literal->cntr=0;
	new_literal->type=type;
	current_literal=new_literal;
}

void pop_literal (void)
{
	_literal * old_literal=NULL;
	free(current_literal->str);
	old_literal=(_literal *)current_literal->prev;
	free(current_literal);
	current_literal=old_literal;
}

/* ----------------------------------------------------------------------
 * get_ch:
 *  Get a character from stdin, place char in line[]
 */

int get_ch(void)
{
    register int c;

	while(current_literal!=NULL)
	{
		c=current_literal->str[current_literal->cntr];
		current_literal->cntr++;
		
		if(c!=0)
		{
			if(current_literal->type==MAC)
			{
				if( c != EOF && lineptr - line < sizeof(line) )
				*lineptr++ = c;
			}
			return(c);
		}
		else
		{
			pop_literal();
		}
	}

	c=fgetc(fin);
    if (last_token_len >=0 && last_token_len < 254 &&
       c != EOF && (last_token_len > 0 || !isspace(c))) {
        last_token[last_token_len++] = c;
    }
    if( c != EOF && lineptr - line < sizeof(line) )
        *lineptr++ = c;
    return(c);
}

/* ----------------------------------------------------------------------
 * unget_ch:
 *  Unget a character and move lineptr back by one.
 */

void unget_ch(int c)
{
	if(current_literal!=NULL)
	{
		if(current_literal->cntr>0) current_literal->cntr--;
		if(current_literal->type==MAC)
		{
			if( lineptr > line ) lineptr--;
		}
	}
	else
	{
		//WARNING: ungetc() can not be called more than 4 times in a row before calling fgetc().
		//From some IBM documentation online: "You can push back as many as 4 characters to a given
		//input stream. You can call the ungetc function as many as four times consecutively; this
		//will result in a total of 4 characters being pushed."  Futhermore, many sources indicate
		//that only one character of ungetc pushback is allowed!

		if(ungetc(c, fin)==EOF) 
		{
			if ( (pass==0) && (feof(fin)==0) )
				fatalerror("ungetc('%c', fin) failed when working on line %d\n", c, lineno);
		}
		if( lineptr > line ) lineptr--;
	}
    
	if (last_token_len>0) last_token_len--;
}

void add_ch(char c)
{
    lineptr++;
    unget_ch(c);
}

int Look_for_Literal (void)
{
    char buf[0x4000];     /* temporary buffer */
	char keyword[0x10];
	char c;
	int j;
	fpos_t pos;

	if(current_literal!=NULL) return 0; // Already inside a literal string, no need to search

	fgetpos(fin, &pos); // Save current position in the file
	j=0;  
	do {
		c=get_ch();
        if( j<sizeof(buf)-1 )
		{
			buf[j] = c;
			j++;
		}
    } while( (c!=EOF) && (c!='\n') && (c!=';'));
    buf[j] = 0;
	fsetpos(fin, &pos); // Restore to original position in the file
	if( lineptr > line ) lineptr-=j;

	keyword[0]=0;
	sscanf(buf, "%15s", keyword);

	if (strcasecmp(keyword, "EQU")==0) return 1;
	if (strcasecmp(keyword, "SET")==0) return 2; // Needed because a 'set' can turn into an 'equ'
	if (strcasecmp(keyword, "MAC")==0) return 3;
	return 0;
}

/* when there is a parse error, yyparse should call here to */
/* advance to the end of the current line, so we can continue */
/* parsing the rest of the file */

int seek_eol(void)
{
    int c;

    last_token_len = -1;
    do {
        c = get_ch();
    } while (c != '\n' && c != EOF);
    if (c == '\n') {
        unget_ch(c);
        return 1;
    }
    return 0;
}

/* return a pointer to the last successfully parsed token, so */
/* we can give a nicer error message when there is a syntax error */

const char *get_last_token(void)
{
    /*
    if (last_token_len < 0) last_token_len = 0;
    last_token[last_token_len] = '\0';
    */
    return prev_token;
}


void do_macro (char * macro)
{
	#define MAX_PARAM 100
	#define PARAM_SIZE 0x100
	#define MACBUFSIZE 0x4000

	char pars_in_macro_text[MAX_PARAM];
	char pars_in_macro_call[MAX_PARAM];
	char param[MAX_PARAM][PARAM_SIZE];
	char * p;
	char c;
	int i, j, k, m, n;
	char inquote=0;
    static char buf[MACBUFSIZE];
	const char buff_is_full[]="Temporary buffer during macro expansion was too small.";
	char linenostr[0x20];
	int parenthesis;


	macro_count++;

	for(n=0; n<MAX_PARAM; n++)
	{
		param[n][0]=0;
		pars_in_macro_text[n]=0;
		pars_in_macro_call[n]=0;
	}

	n=0;
	p = param[n];

	c = get_ch();
	while(( c=='\t' ) || ( c==' ') ) c = get_ch();
	
	if(c!='(')
	{
		unget_ch(c);
	}
	else
	{
		parenthesis=1;
		while((c=get_ch()) != EOF && (parenthesis>0) )
		{
			if(c=='(') parenthesis++;
			if(c==')') parenthesis--;
			
			if(parenthesis>0)
			{
				if( (p-param[n]) < (PARAM_SIZE-1) )
				{
					pars_in_macro_call[n]=1;
					if((c==',') && (inquote==0))
					{
						*p++=0;
						n++;
						if(n>MAX_PARAM)
						{
						   fatalerror("Too many macro parameters (max is %d)", MAX_PARAM);
						   exit(1);
						}
						p = param[n];

						c = get_ch();
						while(( c=='\t' ) || ( c==' ') ) c = get_ch();
						unget_ch(c);
					}
					else
					{
						*p++ = c;
						if(c=='\'')
						{
							inquote++;
							inquote&=1;
						}
					}
				}
				else
				{
				   fatalerror("Macro parameter is longer than %d bytes", PARAM_SIZE);
				   exit(1);
				}
			}
			else
			{
				unget_ch(c);
			}
		}
		*p = '\0';
	}

	// Now replace the macro %1, %2, etc. with the corresponding parameter
	for(j=0, k=0; macro[j]!=0; j++)
	{
		if(macro[j]=='%')
		{
			j++;
			if(isdigit(macro[j]))
			{
				m=macro[j]-'0';
				if(isdigit(macro[j+1]))
				{
					m=m*10+(macro[j+1]-'0');
					j++;
				}
				pars_in_macro_text[m]=1;
				for(i=0; (param[m][i]==' ') || (param[m][i]=='\t'); i++)
				{
					//if(pass==0) printf("Found one spurious blank: '%s'\n", param[m]);
				}
				for(; param[m][i]!=0; i++)
				{
					if(k<(MACBUFSIZE-1)) // Don't overflow the working buffer
					{
						buf[k++]=param[m][i];
					}
					else
					{
						fatalerror("%s", buff_is_full);
					}
				}
			}
			else if (toupper(macro[j])=='L')
			{
				sprintf(linenostr, "%d", lineno);
				for(i=0; linenostr[i]!=0; i++)
				{
					if(k<(MACBUFSIZE-1))
					{
						buf[k++]=linenostr[i];
					}
					else
					{
						fatalerror("%s", buff_is_full);
					}
				}
			}
			else if (toupper(macro[j])=='M')
			{
				sprintf(linenostr, "%d", macro_count);
				for(i=0; linenostr[i]!=0; i++)
				{
					if(k<(MACBUFSIZE-1))
					{
						buf[k++]=linenostr[i];
					}
					else
					{
						fatalerror("%s", buff_is_full);
					}
				}
			}
			else
			{
				if(k<(MACBUFSIZE-2))
				{
					buf[k++]='%';
					buf[k++]=macro[j];
				}
				else
				{
					fatalerror("%s", buff_is_full);
				}
			}
		}
		else
		{
			if(k<(MACBUFSIZE-1))
			{
				buf[k++]=macro[j];
			}
			else
			{
				fatalerror("%s", buff_is_full);
			}
		}
	}
	while(k>0)
	{
		if(buf[k]=='\n')
		{
			buf[k]=0;
			break;
		}
		else
		{
			buf[k]=0;
			k--;
		}
	}

	// Check that the parameters in the macro call and in the macro text match
	for(j=0; j<MAX_PARAM; j++)
	{
		if(pars_in_macro_text[j]!=pars_in_macro_call[j])
		{
			if(pars_in_macro_text[j]==1)
			{
				fatalerror("Parameter '%%%d' in macro text was not passed in macro call.", j);
			}
			else
			{
				fatalerror("Parameter '%%%d' in macro call is not used in macro text.", j);
			}
		}
	}

	lineptr=line; // list the expanded macro
	// Finally, let the lexer play the macro
	push_literal(buf, MAC);
}

/* ----------------------------------------------------------------------
 * yylex:
 *  The tokens themselves are returned via return(token)
 *
 *  Some tokens have attributes. These attributes are returned
 *  by setting a global variable yylval:
 *
 *      yylval.value
 *          numbers (any base)
 *          strings (in pass 1).
 *          bit positions .0, .1, .2, ...
 *
 *      yylval.str
 *          strings (in pass 2).
 *          '\n' (both passes).
 *
 *      yylval.sym
 *          User defined symbols.
 *
 *      yylval.op
 *          Reserved keyword (opcode/directive/misc.)
 *
 *      No other fields in yylval are used by yylex().
 * 
 *      Characters that do not have an attribute do
 *      not set anything in the yylval variable.
 *
 */

int yylex(void)
{
    int c;
    static int nl_flag=0;   /* sync. error messages and the cur. line */
    char buf[0x4000];     /* temporary buffer */
    char *p;        /* general pointer */
    struct symbol *sym;
    struct opcode *op;
	int j;
	char sbuf[0x100];

    int octal=0,hex=0,decimal=0,binary=0;
    register long value = 0;

    if (last_token_len >0)
	{
        last_token[last_token_len] = '\0';
        strcpy(prev_token, last_token);
    }
	else
	{
        *prev_token = 0;
    }
	last_token[0]=0; 
    last_token_len=0;

    if( nl_flag )
	{
        nl_flag = 0;
		line[0]=0;
		if(current_literal==NULL) lineno++;
    }

	if(IF_Level!=0)
	{
		while(IF_Flag[IF_Level]==0)
		{	
			j=0;
			do{
				c=get_ch();
				if( j < (sizeof(buf)-1) )
				{
					buf[j] = c;
					j++;
				}
			} while( (c != EOF) && (c!='\n') );
			buf[j]=0;

			//Get rid of ';' and '('
			for(j=0; buf[j]!=0; j++)
			{
				if(buf[j]==';') buf[j]=0; //Discard comments
				if(buf[j]=='(') buf[j]=' ';
			}
			sbuf[0]=0;
			sscanf(buf, "%254s", sbuf);
			
			if ( (strcasecmp(sbuf, "IF")==0) ||
				 (strcasecmp(sbuf, "IFDEF")==0) ||
				 (strcasecmp(sbuf, "IFNDEF")==0) )
			{
				//Read file until we find a matching ENDIF for the IF/IFDEF/IFNDEF above
				int ifcnt=1;
				while(ifcnt>0)
				{
					j=0;
					do{
						c=get_ch();
						if( j < (sizeof(buf)-1) )
						{
							buf[j] = c;
							j++;
						}
					} while( (c != EOF) && (c!='\n') );
					buf[j]=0;
					if(c=='\n') lineno++;
					
					//Get rid of ';' and '('
					for(j=0; buf[j]!=0; j++)
					{
						if(buf[j]==';') buf[j]=0; //Discard comments
						if(buf[j]=='(') buf[j]=' ';
					}
					sbuf[0]=0;
					sscanf(buf, "%254s", sbuf);
					if ( (strcasecmp(sbuf, "IF")==0) ||
						 (strcasecmp(sbuf, "IFDEF")==0)  ||
						 (strcasecmp(sbuf, "IFNDEF")==0) )	ifcnt++;
					else if (strcasecmp(sbuf, "ENDIF")==0) ifcnt--;
				}
				sbuf[0]=0;
			}

			if (strcasecmp(sbuf, "ENDIF")==0)
			{
				unget_ch(c);
				return(ENDIF);
			}
			if (strcasecmp(sbuf, "ELSE")==0)
			{
				unget_ch(c);
				return(ELSE);
			}
			nl_flag= 1;
			yylval.str = line;
			*lineptr = '\0';
			lineptr = line;
			return(c);
		}
	}

	if(force_EOF!=0)
	{
		if(force_EOF==1) 
		{
			force_EOF++;
			do{
				c=get_ch();
			} while( (c != EOF) && (c!='\n') );
			yylval.str = line;
			*lineptr = '\0';
			lineptr = line;
			if((lineno==linetest)&&(pass==0)) printf("%d: END found\n", lineno);
			return('\n');
		}
		else return(EOF);
	}

	while (1)
	{
		c = get_ch();
		switch(c) {
		case EOF:
			if((lineno==linetest)&&(pass==0)) printf("%d: EOF\n", lineno);
			return(EOF);
		case ' ':
		case '\r':
		case '\t':
			break;

		case '\n':
			nl_flag = 1;
			yylval.str = line;
			*lineptr = '\0';
			lineptr = line;
			if((lineno==linetest)&&(pass==0)) printf("%d: New Line\n", lineno);
			return('\n');

		case ';':
			j=0;
			do{
				c=get_ch();
			} while( (c != EOF) && (c!='\n') );
			unget_ch(c);
			break;

		case '$':
			c=get_ch();
			unget_ch(c);
			if(isalpha(c)==0) return('$');

			p = buf;
			while( (c=get_ch()) != EOF && (c!='\n') )
			{
				if( p-buf<sizeof(buf)-1 )
				{
					*p++ = c;
				}
				else
				{
				   fatalerror("$directive is longer than %d characters",	sizeof(buf));
				}
			}
			*p = '\0';
        
			p = (char *)malloc(strlen(buf) + 1);
			if (p == NULL)
			{
				fatalerror("Cannot allocate %d bytes", strlen(buf) + 1);
				yylval.str = "$abort";
				return(STRING);
			}
			strcpy(p, buf);
			yylval.str = p;
			
			if((lineno==linetest)&&(pass==0)) printf("%d: Control: %s\n", lineno, buf);

			return(CONTROL);

		case '\'':
			p = buf;
			while((c=get_ch()) != EOF && c!='\'' && c!='\n')
			{
				if( c == '\\' )
				{
					switch(c=get_ch())
					{
						case 'n': c = '\n'; break;
						case 'r': c = '\r'; break;
						case 't': c = '\t'; break;
						case 'b': c = '\b'; break;
						case '0': c = 0; break;
						case '\\': c = '\\'; break;
						case '\'': c = '\''; break;
						default:
						  error("Invalid escape character: \\%c",c);
					}
				}
				if( p-buf<sizeof(buf)-1 )
				{
					*p++ = c;
				}
				else
				{
				   fatalerror("String constant longer than %d bytes", sizeof(buf));
				}
			}

			*p = '\0';
			if( c == '\n' || c == EOF )
			{
				error("Missing quote in character constant or string");
				unget_ch(c);
			}

			if(strlen(buf)==1)
			{
				yylval.value = buf[0];
				return(VALUE);
			}
			else
			{
				p = (char *)malloc(strlen(buf) + 1);
				if (p == NULL) {
					fatalerror("Cannot allocate %d bytes",
						strlen(buf) + 1);
					yylval.str = "A51 ran out of memory!";
					return(STRING);
				}
				strcpy(p, buf);
				yylval.str = p;
				if((lineno==linetest)&&(pass==0)) printf("%d: String: %s\n", lineno, buf);
				return(STRING);
			}

		case '.':
			if( (c=get_ch())>='0' && c<='7' )
			{
				yylval.value = c-'0';
				return(BITPOS);
			}
			unget_ch(c);
			if((lineno==linetest)&&(pass==0)) printf("%d: '.'\n", lineno);
			return('.');

		case '0':   /* parse a number           */
		case '1':   /* could be followed by a:      */
		case '2':   /*  'b','B' - Binary        */
		case '3':   /*  'h','H' - Hex           */
		case '4':   /*  'd','D' - Decimal       */
		case '5':   /*  'o','O' - Octal         */
		case '6':   /* *** Numbers must start with a digit  */
		case '7':   /* Numbers could be also preceeded by:  */
		case '8':   /*  0x  - Hex           */
		case '9':   /*  0b  - binary        */

			p = buf;
			do {
				if( p-buf<sizeof(buf)-1 )
					*p++ = c;
				c = get_ch();
			} while( c=='H' || c=='h' ||
					c=='O' || c=='o' ||
					c=='Q' || c=='q' ||
					c=='x' || c=='X' || 
					c=='D' || c=='d' ||
					c=='_' ||
					isxdigit(c) );
			unget_ch(c);
			*p = '\0';

			/* Check any preceeding chars */
			if( buf[0]=='0' && (buf[1]=='x' || buf[1]=='X') )
			{
					hex++;
					buf[1] = '0';
			}
			else if ( buf[0]=='0' && (buf[1]=='q' || buf[1]=='Q' || buf[1]=='o' || buf[1]=='O') )
			{
					octal++;
					buf[1] = '0';
			}
			else if( buf[0]=='0' && (buf[1]=='d' || buf[1]=='D') &&
				  *(p-1) != 'h' && *(p-1) != 'H')
			{
					decimal++;
					buf[1] = '0';
			}
			else
			{
				if( buf[0]=='0' &&
				  (buf[1]=='b' || buf[1]=='B') &&
				  *(p-1) != 'h' && *(p-1) != 'H') {
					binary++;
					buf[1] = '0';
				}
			}

			/* check any trailing chars */
			c = *(p-1);
			if( !hex && (c=='b' || c=='B') )
				{ binary++; *(p-1) = '\0'; }
			else if( c=='H' || c=='h' )
				{ hex++; *(p-1) = '\0'; }
			else if( !hex && (c=='D' || c=='d') )
				{ decimal++; *(p-1) = '\0'; }
			else if( c=='O' || c=='o' )
				{ octal++; *(p-1) = '\0'; }
			else if( c=='Q' || c=='q' )
				{ octal++; *(p-1) = '\0'; }
			else if( !hex && !octal && !binary) decimal++;

			if (binary) {
				if (hex) error("ambiguous number, bin or hex");
				if (decimal) error("ambiguous number, bin or dec");
				if (octal) error("ambiguous number, bin or oct");
				for(p=buf; *p; p++ ) {
					if( *p=='1' ) value = value * 2 + 1;
					else if( *p=='0' ) value = value * 2;
					else if( *p=='_' ) ;
					else
					  error("Invalid binary digit: %c",*p);
				}
				if((lineno==linetest)&&(pass==0)) printf("%d: Binary %XH\n", lineno, value);
				yylval.value = value;
				return(VALUE);
			}

			if (hex) {
				if (binary) error("ambiguous number, hex or bin");
				if (decimal) error("ambiguous number, hex or dec");
				if (octal) error("ambiguous number, hex or oct");
				for(p=buf; *p; p++ ) {
					value <<= 4;
					if( isdigit(*p) )
						value += *p-'0';
					else if( *p>='a' && *p<='f' )
						value += *p-'a'+ 10;
					else if( *p>='A' && *p<='F' )
						value += *p-'A'+ 10;
					else
					  error("Invalid hex digit: %c",*p);
				}
				if((lineno==linetest)&&(pass==0)) printf("%d: Hex %XH\n", lineno, value);
				yylval.value = value;
				return(VALUE);
			}
        
			if (octal) {
				if (hex) error("ambiguous number, oct or hex");
				if (binary) error("ambiguous number, oct or bin");
				if (decimal) error("ambiguous number, oct or dec");
				for(p=buf; *p; p++ ) {
					if( *p>='0' && *p<='7' )
						value = (value << 3) + (*p - '0');
					else
					   error("Invalid octal digit: %c",*p);
				}
				if((lineno==linetest)&&(pass==0)) printf("%d: Octal %oQ\n", lineno, value);
				yylval.value = value;
				return(VALUE);
			}

			if (decimal) {
				if (hex) error("ambiguous number, dec or hex");
				if (binary) error("ambiguous number, dec or bin");
				if (octal) error("ambiguous number, dec or oct");
				for(p=buf; *p; p++ ) {
					if( isdigit(*p) )
						value = value*10 + *p-'0';
					else
					   error("Invalid decimal digit: %c",*p);
				}
				if((lineno==linetest)&&(pass==0)) printf("%d: Decimal %d\n", lineno, value);
				yylval.value = value;
				return(VALUE);
			}

		default:
			if( isalpha(c) || (c=='_') || (c=='?') )
			{
				p = buf;
				do {
					if( p-buf<sizeof(buf)-1 ) *p++ = c;
					c = get_ch();
				} while( isalnum(c) || (c=='_') || (c=='?') );
				*p = '\0';
				unget_ch(c);
				
				if((lineno==linetest)&&(pass==0)) printf("%d: buf is '%s'\n", lineno, buf);

				if ( (op = lookop(buf)) != NULL )
				{

					if(op->type==D_EQU)
					{
						c = get_ch();
						while( isspace(c) ) c = get_ch();
						unget_ch(c);
        
						p = buf;
						while((c=get_ch()) != EOF && c!='\n')
						{
							if( p-buf<sizeof(buf)-1 )
							{
								if(c==';')
								{
									c=0;
								}
								*p++ = c;
							}
							else
							{
							   fatalerror("EQU literal is longer than %d bytes", sizeof(buf));
							   exit(1);
							}
						}
						*p = '\0';
						unget_ch(c);
        
						p = (char *)malloc(strlen(buf) + 1);
						if (p == NULL)
						{
							fatalerror("Cannot allocate %d bytes", strlen(buf) + 1);
							yylval.str = "$abort";
							return(op->type);
						}
						if((lineno==linetest)&&(pass==0)) printf("%d: EQU set to '%s'\n", lineno, buf);
						strcpy(p, buf);
						yylval.str = p;
						return(op->type);
					}
					else if(op->type==D_MAC)
					{
						char n[7]="123456";
						do {
							c=get_ch();
						} while(c!='\n');
						lineno++;

						p = buf;
						while((c=get_ch()) != EOF) // A macro can have more than one line so...
						{
							if(c=='\n') lineno++;

							if( p-buf<sizeof(buf)-1 )
							{
								*p++ = c;
								n[0]=n[1]; n[1]=n[2]; n[2]=n[3];
								n[3]=n[4]; n[4]=n[5]; n[5]=c;
								if(strcasecmp(n, "endmac")==0) break;
							}
							else
							{
							   fatalerror("MACRO is longer than %d bytes", sizeof(buf));
							   exit(1);
							}
						}
						if(c==EOF)
						{
							fatalerror("MAC without ENDMAC");
							exit(1);
						}
						p-=6;
						*p = '\0';
						
						p = (char *)malloc(strlen(buf) + 1);
						if (p == NULL)
						{
							fatalerror("Cannot allocate %d bytes", strlen(buf) + 1);
							yylval.str = "$abort";
							return(op->type);
						}

						strcpy(p, buf);
						if((lineno==linetest)&&(pass==0)) printf("%d: MAC set to '%s'\n", lineno, buf);
						yylval.str = p;
						return(op->type);

					}
					else if ( (op->type==IFDEF) || (op->type==IFNDEF) )
					{
						char mystr[0x100];

						c = get_ch();
						while( isspace(c) ) c = get_ch();
						unget_ch(c);
        
						p = buf;
						while((c=get_ch()) != EOF && c!='\n')
						{
							if( p-buf<sizeof(buf)-1 )
							{
								if(c==';')
								{
									c=0;
								}
								*p++ = c;
							}
							else
							{
							   fatalerror("Conditional text is longer than %d bytes", sizeof(buf));
							   exit(1);
							}
						}
						*p = '\0';
						unget_ch(c);

						mystr[0]=0;
						sscanf(buf, "%254s", mystr);

						sym = looksym(mystr);
        
						if((lineno==linetest)&&(pass==0)) printf("%d: IF(N)DEF for '%s'\n", lineno, mystr);
						yylval.value=(sym->type==UNDEF)?0:1;
						return(op->type);
					}
					else // Not and EQU or MACro
					{
						if((lineno==linetest)&&(pass==0)) printf("%d: TOKEN '%s'\n", lineno, buf);
						yylval.op = op;
						return(op->type);
					}
				}
				else // It is a symbol...
				{
					sym = looksym(buf);
					if( (sym->type==EQU) || (sym->type==MAC) || (sym->type==SET))
					{
						//Check ahead to see if this is the actual definition of the EQU

						if(Look_for_Literal()==0)
						{
							if (sym->type==MAC)
							{
								//Must print here to the list file???
								do_macro(sym->literal);
							}
							else if (sym->type==EQU)
							{
								last_token_len++;
								push_literal(sym->literal, sym->type); //Playback the EQU
							}
							else if (sym->type==SET)
							{
								if(sym->literal!=NULL)
								{
									push_literal(sym->literal, sym->type); //Playback the SET
								}
								else
								{
									if((lineno==linetest)&&(pass==0)) printf("%d: SYMBOL: '%s'\n", lineno, buf);
									yylval.sym = sym;
									return(SYMBOL);
								}
							}
							break;
						}
						else
						{
							if((lineno==linetest)&&(pass==0)) printf("%d: SYMBOL: '%s'\n", lineno, buf);
							yylval.sym = sym;
							return(SYMBOL);
						}
					}
					else // A new symbol, or not an EQU or MAC yet!
					{
						if((lineno==linetest)&&(pass==0)) printf("%d: SYMBOL: '%s'\n", lineno, buf);
						yylval.sym = sym;
						return(SYMBOL);
					}
				}
			}
			else 
			{
				if((lineno==linetest)&&(pass==0))
				{
					printf("%d: '%c'\n", lineno, c);
				}
				return(c);
			}
		} /* switch */
	} /* while (1) */
} /* yylex */


