void
fatal(char* msg)
{
  char* error_msg[100];
  strcpy(error_msg, "[!!] Fatal Error ");
  strncat(error_msg, msg, 83); // 17 bytes for the error msg above - so 100-17 = 83
  perror(error_msg);
  exit(-1);
}

void*
ec_malloc(unsigned int size)
{
  void* ptr;
  ptr = malloc(size);
  if (ptr == NULL)
    fatal("in ec_malloc() on memory allocation");
  return ptr;
}
