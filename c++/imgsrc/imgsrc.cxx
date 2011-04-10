#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>

#include "image_handler.h"
#include "util.h"

void usage(char * prog)
{
  printf("Usage: %s <filename>\n", prog);
}

int main (int argc, char ** argv)
{
  if ( argc < 2 )
  {
    usage(argv[0]);
    return -1;
  }
  char * psz = generate_img_src_string ( argv[1] );
  printf("%s", psz);
  free(psz);
  return 0;
}
