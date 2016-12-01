#include <stdio.h>

int global_var;
int global_initialised_var = 5;

void
function()
{
  int stack_var;
  printf("the function's stack_var is at address 0x%08x\n", &stack_var);
}

int
main()
{
  int stack_var;
  static int static_initialised_var = 5;
  static int static_var;
  int* heap_var_ptr;

  heap_var_ptr = (int*) malloc(4);

  // data segment
  printf("global_initialised_var is at address 0x%08x\n", &global_initialised_var);
  printf("static_initialised_var is at address 0x%08x\n\n", &static_initialised_var);
  // bss segment
  printf("static_var is at address 0x%08x\n", &static_var);
  printf("global_var is at address 0x%08x\n\n", &global_var);
  // heap segment
  printf("heap_var_ptr is at address 0x%08x\n", &heap_var_ptr);
  // stack segment
  printf("stack_var is at address 0x%08x\n\n", &stack_var);
}
