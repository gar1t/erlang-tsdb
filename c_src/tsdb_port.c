#include <unistd.h>
#include <string.h>
#include <stdlib.h>

#include "tsdb_api.h"
#include "erl_interface.h"

typedef unsigned char byte;

#define trace_info(...) traceEvent(TRACE_INFO, __VA_ARGS__);
#define trace_error(...) traceEvent(TRACE_ERROR, __VA_ARGS__);

#define SUCCESS 0

#define REPLY_OK "ok"
#define REPLY_ERROR "error"

#define EXIT_OK 0
#define EXIT_PORT_READ_ERROR 254
#define EXIT_INTERNAL_ERROR 255

#define CMD_BUF_SIZE 10240
#define REPLY_BUF_SIZE 10240

typedef struct {
  tsdb_handler db;
  byte reply_buf[REPLY_BUF_SIZE];
} state;

typedef struct {
  ETERM* pattern;
  void (*handler)(ETERM*, state*);
} cmd_handler;

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

static int read_cmd(int max, byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return -1;
  len = (buf[0] << 8) | buf[1];
  if (len > max) {
    trace_error("command length (%u) > max buf length (%u)", len, max);
    exit(EXIT_INTERNAL_ERROR);
  }
  return read_exact(buf, len);
}

static int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf + wrote, len - wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote < len);

  return len;
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

static void db_init(tsdb_handler *db) {
  db->alive_and_kicking = 0;
}

static void trace_init() {
  traceLevel = 99;
}

static int safe_erl_encode(ETERM *term, int buf_size, byte *buf) {
  int term_len, encoded_len;

  if ((term_len = erl_term_len(term)) > buf_size) {
    trace_error("term_len %u > buf_size %u", term_len, buf_size);
    exit(EXIT_INTERNAL_ERROR);
  }

  if ((encoded_len = erl_encode(term, buf)) != term_len) {
    trace_error("bad result from erl_encode %u, expected %u",
                term_len, encoded_len);
    exit(EXIT_INTERNAL_ERROR);
  }

  return encoded_len;
}

static void write_term(ETERM *term, state *state) {
  int len = safe_erl_encode(term, REPLY_BUF_SIZE, state->reply_buf);
  write_cmd(state->reply_buf, len);
}

static void handle_ping(ETERM *term, state *state) {
  ETERM *pong = erl_format("pong");
  write_term(pong, state);
  erl_free_compound(pong);
}

static void handle_info(ETERM *term, state *state) {
  ETERM *reply;
  if (state->db.alive_and_kicking) {
    reply = erl_format("open");
  } else {
    reply = erl_format("not_open");
  }
  write_term(reply, state);
  erl_free_term(reply);
}

static void handle_cmd(byte *buf, state *state, int handler_count,
                       cmd_handler *handlers) {
  ETERM *term;
  int i, handled = 0;

  term = erl_decode(buf);

  for (i = 0; i < handler_count; i++) {
    if (erl_match(handlers[i].pattern, term)) {
      handlers[i].handler(term, state);
      handled = 1;
      break;
    }
  }

  if (!handled) {
    trace_error("unhandled command");
    erl_print_term(stderr, term);
    exit(EXIT_INTERNAL_ERROR);
  }

  erl_free_compound(term);
}

int main() {
  state state;

  erl_init(NULL, 0);
  db_init(&(state.db));
  trace_init();

  int cmd_len;
  byte cmd_buf[CMD_BUF_SIZE];

  cmd_handler handlers[2];
  handlers[0].pattern = erl_format("ping");
  handlers[0].handler = &handle_ping;
  handlers[1].pattern = erl_format("info");
  handlers[1].handler = &handle_info;

  while (1) {
    cmd_len = read_cmd(CMD_BUF_SIZE, cmd_buf);
    if (cmd_len == 0) {
      exit(EXIT_OK);
    } else if (cmd_len < 0) {
      exit(EXIT_PORT_READ_ERROR);
    } else {
      handle_cmd(cmd_buf, &state, 2, handlers);
    }
  }

  return 0;
}
