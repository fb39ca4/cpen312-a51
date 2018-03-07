/*  run.c

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

#include <stdlib.h>
#include <stdio.h>
#define _POSIX_
#include <limits.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "a51.h"

/* global variables */
int dosyminit=1; // Unless a $modxxx file is loaded, use default symbols
int fatal=0, abort_asap=0;
int pass=0;
int dashl=0;
FILE *listing=NULL, *fin=NULL, *fobj=NULL;
extern int ObjectFile;
extern char FileName[PATH_MAX];
extern char ExeDir[PATH_MAX];
extern char MapFile[PATH_MAX];
extern char optc51[0x1000];
extern int printf_float;

extern int IF_Flag[0x100];
extern int IF_Level;
extern char IF_Value[0x1000];
extern int IF_Count;
extern int macro_count;
extern int force_EOF;

typedef struct
{
   char name[PATH_MAX];
   void * prev;
   int saved_pos;
   int saved_lineno;
   int saved_dashl;
} _source;

_source * current_source;

int run_a51(const char *infile, int lst, int use_stdout,
    const char *fmt, const char *arg)
{
    char asmfile[PATH_MAX];
    char lstfile[PATH_MAX];
    char outfile[PATH_MAX];
    char objfile[PATH_MAX];
	char drive[_MAX_DRIVE];
	char dir[_MAX_DIR];
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];

    current_source=malloc(sizeof(_source));
    if(current_source==NULL)
    {
        printf("ERROR: Not enough memory to run in this system\n");
        exit(-1);
    }
    strcpy(current_source->name, infile);
    current_source->prev=NULL;
    current_source->saved_pos=0;
    current_source->saved_lineno=0;
    current_source->saved_dashl=dashl;

    /* first, figure out all the file names */
	_splitpath(infile, drive, dir, fname, ext);
    strcpy(asmfile, infile);
	_makepath(outfile, drive, dir, fname, "hex" );
	if(strlen(MapFile)==0)
	{
		_makepath(lstfile, drive, dir, fname, "lst" );
		_makepath(objfile, drive, dir, fname, "obj" );
	}
	else
	{
		_makepath(lstfile, drive, dir, fname, "rst" );
		#if defined (__APPLE__ ) || defined (__linux__)
			strcpy(objfile, "/dev/null");
		#else
			strcpy(objfile, "NUL");
		#endif
	}

    /* now open the files */

	fin = fopen(asmfile, "rb");
    if (fin == NULL)
    {
        mesg_f("Cannot open input file: %s\n", asmfile);
        return -1;
    }

	if(ObjectFile==1)
	{
		fobj = fopen(objfile,"w");
		if( fobj == NULL )
		{
			mesg_f("Cannot open file: %s for writing.\n", objfile);
			fclose(fin);
			return -1;
		}
	}

	dashl=1;
    if (dashl)
    {
        listing = fopen(lstfile,"w");
        if( listing == NULL )
        {
            mesg_f("Cannot open file: %s for writing.\n", lstfile);
            fclose(fin);
			if(fobj!=NULL) fclose(fobj);
            return -1;
        }
    }

    /* What happens if this doesn't work?  Nothing really, just the file will not be
	written...  All over emitter.c there are checks to see if the FILE * fout is NULL
	before doing anything.*/
	//if(strlen(MapFile)==0) 
		emitopen(outfile, fmt, arg);

    clearsym(); /* Make sure the symbol table is empty */

    fatal = abort_asap = 0;
    lineno = 1;
    pass=0;
	RegisterBank=0;
	seginit();  // Set the default/absolute segments
	IF_Level=0;
	IF_Count=0;
	macro_count=0;
	

    //if (!use_stdout) mesg_f("Begin Pass #1\n");
    do
    {
        if(current_source->prev!=NULL) pop_include();
		force_EOF=0;
        yyparse();
    } while (current_source->prev!=NULL);

    //If no specfic symbols included put the predefined symbols at the end
    if(dosyminit) syminit();

    if (fatal)
    {
        mesg_f("%d Error%s found, assembly terminated\n", fatal, fatal==1?"":"s");
    }
    else
    {
		//Print the symbol table
		if(ObjectFile)
		{
			make_undeclared_symbols_external();
			fprintf(fobj, "<MODULE>\n");
			fprintf(fobj, "%s\n", fname);
			fprintf(fobj, "</MODULE>\n\n");
			if(strlen(optc51)>0)
			{
				fprintf(fobj, "<OPTC51>\n");
				fprintf(fobj, "%s\n", optc51);
				fprintf(fobj, "</OPTC51>\n\n");
			}
			if(printf_float)
			{
				fprintf(fobj, "<PRINTF_FLOAT>\n");
				if(printf_float) fprintf(fobj, "printf_float\n");
				fprintf(fobj, "</PRINTF_FLOAT>\n\n");
			}
			print_object(fobj);
			fprintf(fobj, "<CODE AT 0000>\n");
		}

        rewind(fin);
        lineno = 1;
        pass++;
		seginit();  // Set the default/absolute segments
        emitaddr(current_seg->value);
		reset_symbol_usage_counter();

		if(strlen(MapFile)>0) // Load symbol values from map file
		{
			LoadSymbolValues(MapFile, fname);
		}

		RegisterBank=0;
		IF_Level=0;
		IF_Count=0;
		macro_count=0;

        //if (!use_stdout) mesg_f("Begin Pass #2\n");
        do
        {
            if(current_source->prev!=NULL) pop_include();
			force_EOF=0;
            yyparse();
        } while (current_source->prev!=NULL);
        
        if (fatal)
        {
            mesg_f("%d Error%s found, assembly terminated\n", fatal, fatal==1?"":"s");
        }
    }

	//Print the symbol table
	//if(ObjectFile) print_object();

	if(ObjectFile)
	{
		fprintf(fobj, "</CODE>\n");
	}

    emitclose();
    fclose(fin);
    if(ObjectFile) fclose(fobj);
    if (dashl) fclose(listing);
    freesym();
    free(current_source);
    if (fatal)
    {
        remove(lstfile);
        remove(outfile);
        remove(objfile);
        return -1;
    }
	else
	{
		if(ObjectFile==0) mesg_f("No errors found\n"); //Only display when generating hex file
	}
    return 0;
}   

void push_include(const char *infile)
{
    char asmfile[PATH_MAX];
	char asmfile2[PATH_MAX]="";
    _source * new_source;
    int j, k;

    if(strncasecmp(infile, "include", 7)==0)
    {
        for(j=0; (infile[j]!='(') && (infile[j]!=0); j++);
        if(infile[j]==0)
        {
            fatalerror("Missing '(' in $INCLUDE control");
            return;
        }
        j++;
        for(k=0; (infile[j]!=')') && (infile[j]!=0); j++, k++)
        {
            asmfile[k]=infile[j];
        }
        asmfile[k]=0;
        if(infile[j]==0)
        {
            fatalerror("Missing ')' in $INCLUDE control");
            return;
        }
    }
    else if(strncasecmp(infile, "mod", 3)==0)
    {
        for(j=0; isalnum(infile[j]); j++) asmfile[j]=infile[j];
        asmfile[j]=0;
    }
    else
    {
        fatalerror("ASSEMBLER FATAL ERROR: Unexpected input in push_include()\n");
        exit(-1);
    }

    new_source=malloc(sizeof(_source));
    if(new_source==NULL)
    {
        fatalerror("ERROR: Not enough memory to run on this system\n");
        exit(-1);
    }

	if(ftell(fin)<0)
	{
        fatalerror("Attempted to save file pointer to %d\nInput file may not be a pure text file\n", ftell(fin));
        exit(-1);
	}

    strcpy(new_source->name, asmfile);
    new_source->prev=(void *)current_source;
    new_source->saved_pos=ftell( fin );
    new_source->saved_lineno=lineno;
    new_source->saved_dashl=dashl;
    current_source=new_source;

    //Leave dashl as it were, making the included file behave
    //as the file that is including it.  This can be changed inside
    //the included file by using $list and $nolist.

    lineno=0;
    fclose(fin);
    fin = fopen(asmfile, "rb");
    if (fin == NULL)
    {
		//Ok, that didn't work.  Try looking in the same path where the assembler lives
		strcpy(asmfile2, ExeDir);
		strcat(asmfile2, asmfile);
		fin = fopen(asmfile2, "rb");
		if(fin==NULL)
		{
			//Ok, that didn't work either.  Try the 'Define' folder
			strcpy(asmfile2, ExeDir);
			strcat(asmfile2,"..\\Define\\");
			strcat(asmfile2, asmfile);
			fin = fopen(asmfile2, "rb");
			if(fin==NULL)
			{
				fatalerror("FATAL ERROR: Cannot open include file: '%s'\n", asmfile);
				exit(-1);
			}
		}
    }
	//if(strlen(asmfile2)>0) printf("Included file: %s\n", asmfile2);
}

void pop_include(void)
{
    _source * prev_source;
    prev_source=(_source *)current_source->prev;

    fclose(fin);
    fin = fopen(prev_source->name, "rb");
    if (fin == NULL)
    {
        fatalerror("FATAL ERROR: Cannot re-open file: %s\n", prev_source->name);
        exit(-1);
    }
    else
    {
        fseek( fin, current_source->saved_pos, SEEK_SET );
    }
    lineno=current_source->saved_lineno+1;
    dashl=current_source->saved_dashl;
    free(current_source);
    current_source=prev_source;
}   

//Remove all '\n' and '\r' from a string
void clean_str (char * buf)
{
	int j, k, len;

	len=strlen(buf);
	
	for(j=0, k=0; j < len; j++)
	{
		if( (buf[j]!='\n') && (buf[j]!='\r'))
		{
			buf[k++]=buf[j];
		}
	}
	buf[k++]='\n';
	buf[k]=0;
}

/* the parser, lexer and other stuff that actually do the */
/* assembly will call to these two functions to report any */
/* errors or warning.  error() calls exit() in the command */
/* line version, but the abort_asap flag was added, and the */
/* parser check it at the end of every line */

void fatalerror(const char *fmt, ...)
{
    va_list args;
    char buf[2048];
    int len;

    abort_asap++;
    fatal++;
    va_start(args, fmt);

    len = snprintf(buf, sizeof(buf), "%s:%d: FATAL ERROR ", current_source->name, lineno);
    len += vsnprintf(buf + len, sizeof(buf) - len, fmt, args);
	clean_str(buf);
    mesg(buf);
}


void error(const char *fmt, ...)
{
    va_list args;
    char buf[2048];
    int len;

    fatal++;
    va_start(args, fmt);

    len = snprintf(buf, sizeof(buf), "%s:%d: ERROR: ", current_source->name, lineno);
    len += vsnprintf(buf + len, sizeof(buf) - len, fmt, args);
	clean_str(buf);
    mesg(buf);
}

void warning(const char *fmt, ...)
{
    va_list args;
    char buf[2048];
    int len;

    va_start(args, fmt);

    len = snprintf(buf, sizeof(buf), "%s:%d: WARNING: ", current_source->name, lineno);
    len += vsnprintf(buf + len, sizeof(buf) - len, fmt, args);
	clean_str(buf);
    mesg(buf);
}


void mesg_f(const char *fmt, ...)
{
    va_list args;
    char buf[2048];

    va_start(args, fmt);

    vsnprintf(buf, sizeof(buf), fmt, args);
    mesg(buf);
}

