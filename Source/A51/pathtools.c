/*  pathtools.c

    Copyright (C) 2010, 2012  Jesus Calvino-Fraga, jesusc (at) ece.ubc.ca
    
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

/* Most of the code below was found on the internet, unfortunatelly I couldn't find
the name of the original author. */

#include <stdio.h>
#include <string.h>

/* windows example = "C:\Documents and Settings\User\My Documents\Digi_ESP\blah.ggg"; */
/* unix example ? = "/usr/bin/blah.ggg" */
/* unix example2 ? = "/durp <-- just a drive name */

#define GetLastChar(x) strchr(x, '\0')
#define NullTerminate(x) x[i] = '\0'

void mysplitpath(const char *path, char *drive, char *dir, char *fName, char *ext)
{
    char separator = '\\';

	char *endPoint = NULL,
	*pos = (char*) path,
	*temp = NULL,
	*lastChar = NULL;
	
	unsigned int i = 0,
	unixStyle = 0;
	
	/* initialize all the output strings in case we have to abort */
	if(drive) { strcpy(drive, ""); }
	if(dir) { strcpy(dir, ""); }
	if(fName) { strcpy(fName, ""); }
	if(ext) { strcpy(ext, ""); }

	/* find the end of the string */
	lastChar = GetLastChar(path);
	
	if(path[0] == '/')
	{
		separator = '/';
		unixStyle = 1;
	}
	else
	{
		separator = '\\';
	}

	/* first figure out whether it contains a drive name */
	endPoint = strchr(path, separator);
	
	/* unix style drives are of the form "/drivename/" */
	if(unixStyle)
	{
		endPoint = strchr(endPoint + 1, separator);
	}
	
	/* we found a drive name */
	if(endPoint && (endPoint < lastChar))
	{
		if(drive)
		{
			for(i = 0; pos + i < endPoint; i++)	drive[i] = pos[i];
			NullTerminate(drive); /* null terminate the the drive string */
		}

		pos = endPoint;
	}
	else if(unixStyle)
	{

		if(drive)
        {
        	for(i = 0; (pos + i) < lastChar; i++) drive[i] = pos[i];
			NullTerminate(drive);
		}

		return;
	}
	else /* this happens when there's no separators in the path name */
	{
		endPoint = pos;
	}

    /* next, find the directory name, if any */
    temp = pos;

    while(temp && (endPoint < lastChar) )
    {
        temp = strchr(endPoint + 1, separator);

        if(temp)
        {
            endPoint = temp;
        }
    }

    /* if true, it means there's an intermediate directory name */
    if( (endPoint) && (endPoint > pos) && (endPoint < lastChar))
    {
        if(dir)
        {
            for(i = 0; (pos + i) <= endPoint; i++)
                dir[i] = pos[i];

            NullTerminate(dir);
        }

        pos = ++endPoint;
    }
    else /* this happens when there's no separators in the path name */
    {
        endPoint = pos;
    }

    /* find the file name */
    temp = pos;

    while(temp && (endPoint < lastChar))
    {
        temp = strchr(endPoint + 1, '.');

        if(temp)
        {
            endPoint = temp;
        }
    }

    if( (endPoint > pos) && (endPoint < lastChar))
    {
        if(fName)
        {
            for(i = 0; pos + i < endPoint; i++)
                fName[i] = pos[i];

            NullTerminate(fName);
        }

        pos = endPoint;
    }
    else if(endPoint == pos)
    {
        /* in this case there is no extension */
        if(fName)
        {
            for(i = 0; (pos + i) < lastChar; i++)
                fName[i] = pos[i];

            fName[i] = '\0';
        }

        return;
    }

    /* the remaining characters just get dumped as the extension */
    if(ext)
    {
        for(i = 0; pos + i < lastChar; i++)
            ext[i] = pos[i];

        NullTerminate(ext);
    }

}

/* example of the kind of things you might have to deal with (though unlikely):
drive = "C:\"
dir = "\MyDocuments\"
fName = "budget"
ext = "doc" (not the need to add the dot)
*/

void mymakepath(char *path, const char *drive, const char *dir, const char *fName, const char *ext)
{
    char separator = '\\';
	char *lastChar = NULL;
	char *pos = NULL;
	
	unsigned int i = 0,
	unixStyle = 0,
	sepCount = 0; /* number of consecutive separators */
	
	
	if(!path)
	return;
	
	/* Initialize the path to nothing */
	strcpy(path, "");
	
	if(drive)
	{
		if(drive[0] == '/')
		{
			unixStyle = 1; separator = '/';
		}
	
		sepCount = 0;
		pos = (char*) drive;
		lastChar = GetLastChar(drive);
	
		if(lastChar == pos)
		goto directory;
	
		for(; pos < lastChar; pos++)
		{
			sepCount = ( (*pos) == separator ) ? sepCount + 1 : 0;
			
			/* filter out any extra separators */
			if(sepCount > 1) { continue; }
			
			path[i++] = (*pos);
		}
	
		if( (i) && path[i-1] != separator)
		path[i++] = separator;
	
		NullTerminate(path);
	}
	
	directory:
	
	if(dir)
	{
		sepCount = 0;
		pos = (char*) dir;
		lastChar = GetLastChar(dir);
		
		if(pos == lastChar)
		goto fileName;
	
		/* no character in the path yet? have to add that first separator */
		if(!i)	path[i++] = separator; sepCount++;
		
		/* getting rid of any extra separators */
		while( ((*pos) == separator) && (pos < lastChar) )	pos++;
	
		for( ; pos < lastChar; pos++)
		{
			sepCount = ( (*pos) == separator ) ? sepCount + 1 : 0;
			if(sepCount > 1) { continue; }
			path[i++] = (*pos);
		}
	
		if( (i) && path[i-1] != separator)
		path[i++] = separator;
	
		NullTerminate(path);
	}
	
	fileName:
	
	if(fName)
	{
		pos = (char*) fName;
		lastChar = GetLastChar(fName);
		
		if(lastChar == pos)
		goto extension;
		
		for(sepCount = 0; pos < lastChar; pos++)
		{
			sepCount = ( (*pos) == '.' ) ? sepCount + 1 : 0;
			if(sepCount > 1) { continue; }
			path[i++] = (*pos);
		}
		
		NullTerminate(path);
	}
	
	extension:
	
	if(ext)
	{
		sepCount = 0;
		pos = (char*) ext;
		lastChar = GetLastChar(ext);
		
		if(lastChar == pos)
		return;
		
		if(i && (path[i - 1] != '.'))
		{ path[i++] = '.'; sepCount++; }
		
		for(; pos < lastChar; pos++)
		{
			sepCount = ( (*pos) == '.' ) ? sepCount + 1 : 0;
			if(sepCount > 1) { continue; }
			path[i++] = (*pos);
		}
		
		NullTerminate(path);
	}
	else
	{
		lastChar = GetLastChar(path) - 1;
		
		/* backpedal until we get rid of all the dots b/c what's the use of a dot on an extensionless file? */
		while(lastChar > path)
	    {
	        if((*lastChar) != '.')
	            break;
	
	        (*lastChar) = '\0';
	        lastChar--;
	    }
	}
}

#undef GetLastChar
#undef NullTerminate
