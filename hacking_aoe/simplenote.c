#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

void
usage(char* prog_name, char* filename)
{
  printf("Usage :% <data to add to %>\n", prog_name, filename);
  exit(0);
}

void
fatal(char*);

void*
ec_malloc(unsigned int);

int
main(int argc, char* argv[])
{
  int fd;
  char* buffer;
  char* datafile;

  buffer = (char*) ec_malloc(100);
  datafile = (char*) ec_malloc(20);
  strcpy(datafile, "/tmp/notes");

  if (argc<2)
    usage(argv[0], datafile);
  strcpy(buffer, argv[1]);
  printf("[DEBUG] buffer\t\t@ %p: '%s'\n", buffer, buffer);
  printf("[DEBUG] datafile\t@ %p: '%s'\n", datafile, datafile);

  strncat(buffer, "\n", 1);

  fd = open(datafile, O_WRONLY|O_CREAT|O_APPEND, S_IRUSR|S_IWUSR);
  if (fd==-1)
    fatal("in main() while opening file");
  if (write(fd, buffer, strlen(buffer)) == -1)
    fatal("in main() while writing buffer to file");
  if (close(fd) == -1)
    fatal("in main() while closing file");

  printf("Note has been saved.\n");
  free(buffer);
  free(datafile);
}

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
