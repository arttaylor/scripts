#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "image_handler.h"
#include "util.h"


Dimension * build_Dimension(unsigned int width, unsigned int height)
{
  Dimension * pdTmp = (Dimension*) malloc(sizeof(Dimension));
  pdTmp->width = width;
  pdTmp->height = height;
  return pdTmp;
}

Dimension * handle_jpeg(char * buf, unsigned int size)
{
  unsigned short height = 0, width = 0;

  for ( unsigned int i = 0; i < size; )
  {
    if ( buf[i++] == (char)0xff )
    {
      if ( buf[i] == (char)0xc0 || buf[i] == (char)0xc2 )
      {
	i++; // go to next byte
	i+=2; // skip record length ( 2 bytes )
	i++; // skip precision;
	height = (unsigned char)(buf[i]);
	height <<= 8;
	height += (unsigned char)(buf[i+1]);
	i+=2;
	width = (unsigned char)(buf[i]);
	width <<= 8;
	width += (unsigned char)(buf[i+1]);
	i+=2;
      }
    }
  }

  return build_Dimension(width, height);
}

Dimension * handle_pcx(char * buf, unsigned int size)
{
  unsigned short height = 0, width = 0;
  
  width = (unsigned char)(buf[9]);
  width <<= 8;
  width += (unsigned char)(buf[8]);
  
  height = (unsigned char)(buf[11]);
  height <<= 8;
  height += (unsigned char)(buf[10]);
  
  return build_Dimension(width, height);
}

Dimension * handle_gif(char * buf, unsigned int size)
{
  unsigned short height = 0, width = 0;

  width = (unsigned char)(buf[7]);
  width <<= 8;
  width += (unsigned char)(buf[6]);
  
  height = (unsigned char)(buf[9]);
  height <<= 8;
  height += (unsigned char)(buf[8]);
  
  return build_Dimension(width, height);
}

Dimension * handle_png(char * buf, unsigned int size)
{
  int height = 0, width = 0;
  
  char * pszIhdr = buf;
  char * pszEnd = buf + size;
  
  while ( pszIhdr < pszEnd)
    {
      if ( strncmp(pszIhdr, "IHDR", 4) == 0)
	break;
      pszIhdr++;
    }
  
  if ( pszIhdr != NULL )
  {
    width = (((unsigned int)pszIhdr[4]) << 24) + 
      (((unsigned int)pszIhdr[5]) << 16) +
      (((unsigned int)pszIhdr[6]) << 8) +
      ((unsigned int)pszIhdr[7]);
    height = (((unsigned int)pszIhdr[8]) << 24) + 
      (((unsigned int)pszIhdr[9]) << 16) +
      (((unsigned int)pszIhdr[10]) << 8) +
      ((unsigned int)pszIhdr[11]);
  }

  return build_Dimension(width, height);
}

handler get_handler(char * pszFileName)
{
  char * pszExt = strrchr(pszFileName, '.');
  Dimension * ( * image_handler ) (char * pszFileName, unsigned int size);
  
  image_handler = NULL;
  
  if ( strcasecmp(pszExt+1, "jpg") == 0 || strcasecmp(pszExt+1, "jpeg") == 0)
    image_handler = handle_jpeg;
  
  if ( strcasecmp(pszExt+1, "pcx") == 0 )
    image_handler = handle_pcx;
  
  if ( strcasecmp(pszExt+1, "gif") == 0 )
    image_handler = handle_gif;
  
  if ( strcasecmp(pszExt+1, "png") == 0 )
    image_handler = handle_png;
  
  return image_handler;
}

