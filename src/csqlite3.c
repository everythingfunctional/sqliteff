/*
MODULE csqlite3

USE sqlite3
*/

#include "sqlite3.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

int csqlite3_bind_double(sqlite3_stmt **stmt, int col, double val) {
  return sqlite3_bind_double(*stmt, col, val);
}

int csqlite3_bind_int(sqlite3_stmt **stmt, int col, int val) {
  return sqlite3_bind_int(*stmt, col, val);
}

int csqlite3_bind_text(sqlite3_stmt **stmt, int col, char *string, int nByte) {
  return sqlite3_bind_text(*stmt, col, string, nByte, SQLITE_TRANSIENT);
}

int csqlite3_clear_bindings(sqlite3_stmt **stmt) {
  return sqlite3_clear_bindings(*stmt);
}

int csqlite3_close(sqlite3 **db) { return sqlite3_close(*db); }

int csqlite3_column_count(sqlite3_stmt **stmt) {
  return sqlite3_column_count(*stmt);
}

double csqlite3_column_double(sqlite3_stmt **stmt, int col) {
  return sqlite3_column_double(*stmt, col);
}

int csqlite3_column_int(sqlite3_stmt **stmt, int col) {
  return sqlite3_column_int(*stmt, col);
}

void csqlite3_column_text(sqlite3_stmt **stmt, int col, char *text,
                          int max_len) {
  const char *string;
  string = sqlite3_column_text(*stmt, col);
  strncpy(text, string, max_len);
}

int csqlite3_exec(sqlite3 **db, char *command, char *errmsg, int max_len) {
  char *message;
  int status;
  status = sqlite3_exec(*db, command, NULL, 0, &message);
  if (status == SQLITE_OK) {
    strncpy(errmsg, "\0", max_len);
  } else {
    strncpy(errmsg, message, max_len);
  }
  return status;
}

int csqlite3_finalize(sqlite3_stmt **stmt) { return sqlite3_finalize(*stmt); }

int csqlite3_last_insert_rowid(sqlite3 **db) {
  return sqlite3_last_insert_rowid(*db);
}

int csqlite3_open(char *filename, sqlite3 **db) {
  return sqlite3_open(filename, db);
}

int csqlite3_prepare(sqlite3 **db, const char *zSql, int nByte,
                     sqlite3_stmt **ppStmt, char *pzTail, int max_len) {
  const char *remaining;
  int status;
  status = sqlite3_prepare(*db, zSql, nByte, ppStmt, &remaining);
  strncpy(pzTail, remaining, max_len);
  return status;
}

int csqlite3_reset(sqlite3_stmt **stmt) { return sqlite3_reset(*stmt); }

int csqlite3_step(sqlite3_stmt **stmt) { return sqlite3_step(*stmt); }
