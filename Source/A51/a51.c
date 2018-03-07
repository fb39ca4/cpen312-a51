/*  a51.c

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

int ObjectFile=0;  // By default, do not generate object file.  Generate hex file automatically.
int CaseSensitive_Flag=0; // By default, ignore case of symbols

char FileName[PATH_MAX];
char ExeDir[PATH_MAX];
char MapFile[PATH_MAX];

void GetDirFromPath(char * path, char * dir)
{
    int i;

	strcpy(dir, path);
    for(i=strlen(dir); i>=0; i--)
    {
    	if((dir[i]=='\\') || (dir[i]=='/'))
		{
			dir[i+1]=0;
			break;
		}
    }
}


int main(int argc, char **argv)
{
    char *dashF=NULL, *dashA=NULL;
    int use_stdout=0, do_lst=0;
    int r, i;

	GetDirFromPath(argv[0], ExeDir);
	//printf("ExeDir: %s\n", ExeDir);
	
	MapFile[0]=0;

    if (argc < 2) {
        fprintf(stderr,
            "Usage: %s [-l] [-s] [-Ffmt] [-Aarg] file.asm\n",
            argv[0]);
        fprintf(stderr, "\t -l :  create list file\n");
        fprintf(stderr, "\t -s :  send output to stdout\n");
        fprintf(stderr, "\t -F :  output format (intel hex default)\n");
        fprintf(stderr, "\t -A :  optional output format argument\n");
        fprintf(stderr, "\t -i :  case sensitive symbols\n");
        fprintf(stderr, "\t -c :  compile only.  Generates object file.\n");
        fprintf(stderr, "\t -rmapfile :  recreate list file using values from 'mapfile'\n");
        emitusage();
        exit(1);
    }

    for (i=1; i<argc; i++ )
	{
        if( argv[i][0] != '-' ) break;
        if( argv[i][1] == 'l' )
		{
            do_lst = 1;
		}
        else if( argv[i][1] == 's' )
		{
            use_stdout = 1;
		}
        else if( dashF == NULL && argv[i][1] == 'F' )
		{
            dashF = argv[i]+2;
		}
        else if( dashA == NULL && argv[i][1] == 'A' )
		{
            dashA = argv[i]+2;
		}
        else if( argv[i][1] == 'c' )
		{
            ObjectFile=1;
		}
        else if( argv[i][1] == 'i' )
		{
            CaseSensitive_Flag=1;
		}
		else if( argv[i][1] == 'r' ) // Update list file with mapfile values
		{
			// This option is only valid for relocatable (i.e. object files) and
			// case insensitive cases.  So:
			ObjectFile=1;
			CaseSensitive_Flag=1;
			strcpy(MapFile, &argv[i][2]);
		}
        else {
            fprintf(stderr,"Duplicate or unknown flag.\n");
            exit(1);
        }
    }

    if (i == argc)
	{
        fprintf(stderr,"Missing input file.\n");
        exit(1);
    }

	strcpy(FileName, argv[i]); 
    r = run_a51(FileName, do_lst, use_stdout, dashF, dashA);
    return r;
}


/* the assembler calls here to display any messages */

void mesg(const char *str)
{
    if (str == NULL) str = "(null)";
    fprintf(stderr, "%s", str);
}



