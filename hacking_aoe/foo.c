#include <stdio.h>
#include <limits.h>


int 
adder(unsigned int x, unsigned int y)
{
  unsigned int i = 0xAAAAAAAA;
  i = x + y;
  return i;
}

int
main()
{
  unsigned int ret = 0xBBBBBBBB;
  ret = adder(2, 3);
  printf("Adder returned: %d\n", ret);
  
  return 0;
}
