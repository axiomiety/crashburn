#include <stdio.h>
#include <string.h>

int main()
{
  char str_a[20];
  char* ptr;
  char* ptr2;

  strcpy(str_a, "Hello, world!\n");
  ptr = str_a;
  printf(ptr);

  ptr2 = ptr + 2;
  printf(ptr2);

  strcpy(ptr2, "y you guys!\n");
  printf(ptr);
}
