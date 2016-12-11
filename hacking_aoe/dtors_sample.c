#include <stdio.h>
#include <stdlib.h>

static void cleanup(void) __attribute__ ((destructor));

main()
{
  printf("Some actions happen in the main() fn...\n");
  printf("and then when main() exists, the destructor is called.\n");
  exit(0);
}

void
cleanup(void)
{
  printf("In cleanup\n");
}
