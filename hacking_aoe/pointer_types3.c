#include <stdio.h>
#include <string.h>

int main()
{
  int i;
  char char_array[5] = {'a','b','c','d','e'};
  int int_array[5] = {1,2,3,4,5};

  char* char_ptr;
  int* int_ptr;

  char_ptr = (char*) int_array;
  int_ptr = (int*) char_array;

  for(i=0;i<5;i++)
  {
    printf("[int ptr] points to %p, which contains the char '%c'\n",
        int_ptr,
        *int_ptr);
    int_ptr = (int*) ((char*) int_ptr + 1);
  }
  for(i=0;i<5;i++)
  {
    printf("[char ptr] points to %p, which contains the integer %d\n",
        char_ptr,
        *char_ptr);
    char_ptr = (char*) ((int*) int_ptr + 1);
  }
}
