#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "image_handler.h"
#include "util.h"


char * unquote_filename(char * pszIn)
{
  if ( pszIn[0] == '\"' )
    pszIn++;
  
  char * last = strrchr(pszIn, '\"');
  
  if ( last )
    *last = 0;
  
  return pszIn;
}

unsigned int get_file_size(char * pszFile)
{
  struct stat st;
  
  stat ( pszFile, &st);
  
  return st.st_size;
}

char * suck_file_into_buffer ( char * pszFile, char ** buf, unsigned int *size )
{
  pszFile = unquote_filename(pszFile);
  
  *size = get_file_size(pszFile);
  
  FILE * pfIn = fopen (pszFile, "rb");
  
  *buf = (char *) malloc(*size);
  
  if ( *buf == NULL )
  {
    char pszError[1024];
    sprintf(pszError, "ERROR: [%s] :: malloc(%u) returned NULL", pszFile, size);
    printf("%s\n", pszError);
    exit(-1);
  }
  
  int cRead = fread(*buf, *size, 1, pfIn);
  
  if ( cRead == 0 )
  {
    if ( feof(pfIn) )
    {
      printf("%s\n", "feof()\n");
    }
    else
    {
      char pszError[1024];
      sprintf(pszError, "ferror() returned %d\n", ferror(pfIn));
      printf("%s\n", pszError);
    }
    exit(-1);
  }

  fclose(pfIn);

  return *buf;
}

char * basename ( char * pszFileName )
{
  char * pszBaseName = strrchr(pszFileName, '\\');

  if ( pszBaseName == NULL )
    pszBaseName = pszFileName;
  else
    pszBaseName++;
  return pszBaseName;
}

char * generate_img_src_string ( char *pszFileName)
{
  char * buf;
  unsigned int size = 0;
  
  suck_file_into_buffer(pszFileName, &buf, &size);

  Dimension * pdImg = NULL;
  Dimension * ( * image_handler ) (char * pszFileName, unsigned int size) = get_handler(pszFileName);
  
  if ( image_handler )
    pdImg = (*image_handler)(buf, size);
  
  free(buf);
  
  char * pszImgSrcString = (char*)malloc(1024);
  sprintf(pszImgSrcString, "<img src=\"%s\" width=%d height=%d>", 
	  basename(pszFileName), pdImg->width, pdImg->height);
  free(pdImg);
  return pszImgSrcString;
}
