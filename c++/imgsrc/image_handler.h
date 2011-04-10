typedef struct
{
  unsigned int width;
  unsigned int height;
} Dimension;

typedef Dimension * (*handler)(char*, unsigned int);

handler get_handler(char * pszFileName);
