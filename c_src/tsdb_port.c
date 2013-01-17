#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "erl_interface.h"
#include "tsdb_api.h"

typedef unsigned char byte;

static int foo(int x) {
  return x + 1;
}

static int bar(int y) {
  return y * 2;
}

static int read_exact(byte *buf, int len)
{
  int i, got = 0;

  do {
    if ((i = read(0, buf + got, len - got)) <= 0)
      return i;
    got += i;
  } while (got < len);

  return len;
}

static int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return -1;
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

static int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

static int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);

  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

static void init_db(tsdb_handler *db) {
  db->alive_and_kicking = 0;
}

int main() {
  tsdb_handler db;
  init_db(&db);

  ETERM *tuplep, *intp;
  ETERM *fnp, *argp;
  int res;
  byte buf[100];

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);

    if (strncmp(ERL_ATOM_PTR(fnp), "foo", 3) == 0) {
      res = foo(ERL_INT_VALUE(argp));
    } else if (strncmp(ERL_ATOM_PTR(fnp), "bar", 17) == 0) {
      res = bar(ERL_INT_VALUE(argp));
      if (res > 100) {
        exit(1);
      }
    }

    intp = erl_mk_int(res);
    erl_encode(intp, buf);
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
    erl_free_term(intp);
  }

  return 0;
}
