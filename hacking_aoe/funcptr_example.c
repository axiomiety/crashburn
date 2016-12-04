#include <stdio.h>

int
func_one()
{
  printf("This is function one.\n");
  return 1;
}

int
func_two()
{
  printf("This is function two.\n");
  return 2;
}

int
main()
{
  int value;
  int (*fn_ptr) ();

  fn_ptr = func_one;
  printf("fn_ptr is 0x%08x\n", fn_ptr);
  value = fn_ptr();
  printf("value returned was %d\n", value);
  
  fn_ptr = func_two;
  printf("fn_ptr is 0x%08x\n", fn_ptr);
  value = fn_ptr();
  printf("value returned was %d\n", value);
}
