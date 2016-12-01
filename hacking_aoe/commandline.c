#include <stdio.h>
#include <string.h>

int main(int arg_count, char* arg_list[])
{
  int i;
  printf("There were %d arguments provided:\n", arg_count);
  for (i=0;i<arg_count;i++)
    printf("arguemnt #%d\t-\t%s\n", i, arg_list[i]);
}
