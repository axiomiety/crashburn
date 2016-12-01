#include <stdio.h>
#include <string.h>

int main()
{
  char msg[10];
  int count, i;
  strcpy(msg, "Hello, world!\n");
  printf("Repeate how many times? ");
  scanf("%d", &count);
  for (i=0; i<count; i++)
    printf("%3d - %s", i, msg);
}
