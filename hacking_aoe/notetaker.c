#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "hacking.h"

void
usage(char* prog_name, char* filename)
{
  printf("Usage :% <data to add to %>\n", prog_name, filename);
  exit(0);
}

int
main(int argc, char* argv[])
{
  int fd;
  int userid;
  char* buffer;
  char* datafile;

  buffer = (char*) ec_malloc(100);
  datafile = (char*) ec_malloc(20);
  strcpy(datafile, "/var/notes");

  if (argc<2)
    usage(argv[0], datafile);
  strcpy(buffer, argv[1]);
  printf("[DEBUG] buffer\t\t@ %p: '%s'\n", buffer, buffer);
  printf("[DEBUG] datafile\t@ %p: '%s'\n", datafile, datafile);

  strncat(buffer, "\n", 1);

  fd = open(datafile, O_WRONLY|O_CREAT|O_APPEND, S_IRUSR|S_IWUSR);
  if (fd==-1)
    fatal("in main() while opening file");
  printf("[DEBUG] file descriptor is %d\n", fd);
  userid = getuid();

  if (write(fd, &userid, 4) == -1)
    fatal("in main() while writing userid to file");
  write(fd, "\n", 1);
  if (write(fd, buffer, strlen(buffer)) == -1)
    fatal("in main() while writing buffer to file");
  write(fd, "\n", 1);
  if (close(fd) == -1)
    fatal("in main() while closing file");

  printf("Note has been saved.\n");
  free(buffer);
  free(datafile);
}

