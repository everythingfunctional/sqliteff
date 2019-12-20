/*
MODULE csqlite3
*/

#include "sqlite3.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

int csqlite3_close(sqlite3 **db) { return sqlite3_close(*db); }

int csqlite3_exec(sqlite3 **db, char *command, char *errmsg, int maxlen) {
  char *message;
  int status;
  status = sqlite3_exec(*db, command, NULL, 0, &message);
  if (status == SQLITE_OK) {
    strncpy(errmsg, "\0", maxlen);
  } else {
    strncpy(errmsg, message, maxlen);
  }
  return status;
}

int csqlite3_open(char *filename, sqlite3 **db) {
  return sqlite3_open(filename, db);
}
