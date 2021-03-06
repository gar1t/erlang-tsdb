// Copyright (C) 2012, 2013 Garrett Smith <g@rre.tt>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
  traceLevel = 0;
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
  erl_free_term(pong);
}

static void handle_info(ETERM *term, state *state) {
  ETERM *reply;
  if (state->db.alive_and_kicking) {
    char *read_only;
    if (state->db.read_only_mode) {
      read_only = "true";
    } else {
      read_only = "false";
    }
    reply = erl_format("{ok,["
                       "{slot_seconds,~i}"
                       "{values_per_entry,~i},"
                       "{read_only,~a}]}",
                       state->db.rrd_slot_time_duration,
                       state->db.num_values_per_entry,
                       read_only);
  } else {
    reply = erl_format("{error,not_open}");
  }
  write_term(reply, state);
  erl_free_term(reply);
}

static int check_db_not_opened(state *state) {
  if (state->db.alive_and_kicking) {
    ETERM *error = erl_format("{error,already_open}");
    write_term(error, state);
    erl_free_term(error);
    return 1;
  } else {
    return 0;
  }
}

static void handle_open(ETERM *term, state *state) {
  if (check_db_not_opened(state)) {
    return;
  }

  ETERM *file_arg = erl_element(2, term);
  ETERM *slot_seconds_arg = erl_element(3, term);
  ETERM *values_per_entry_arg = erl_element(4, term);
  ETERM *read_only_arg = erl_element(5, term);

  char *file = erl_iolist_to_string(file_arg);
  u_int32_t slot_seconds = ERL_INT_UVALUE(slot_seconds_arg);
  u_int16_t values_per_entry = ERL_INT_UVALUE(values_per_entry_arg);
  u_int8_t read_only = ERL_INT_UVALUE(read_only_arg);

  ETERM *reply;
  int rc;

  rc = tsdb_open(file, &state->db, &values_per_entry, slot_seconds, read_only);
  if (rc) {
    reply = erl_format("{error,{tsdb_open,~i}}", rc);
  } else {
    reply = erl_format("ok");
  }

  write_term(reply, state);

  erl_free(file);
  erl_free_term(reply);
}

static void handle_close(ETERM *pattern, state *state) {
  tsdb_close(&state->db);
  if (state->db.alive_and_kicking) {
    trace_error("assertion failed: db.alive_and_kicking != 0");
    exit(EXIT_INTERNAL_ERROR);
  }

  ETERM *reply = erl_format("ok");
  write_term(reply, state);
  erl_free_term(reply);
}

static int check_db_opened(state *state) {
  if (!state->db.alive_and_kicking) {
    ETERM *error = erl_format("{error,not_open}");
    write_term(error, state);
    erl_free_term(error);
    return 1;
  } else {
    return 0;
  }
}

static void handle_goto(ETERM *term, state *state) {
  if (check_db_opened(state)) {
    return;
  }

  ETERM *epoch_arg = erl_element(2, term);

  u_int32_t epoch = ERL_INT_UVALUE(epoch_arg);
  u_int8_t create_if_needed = 1;
  u_int8_t growable = 1;
  u_int8_t load_page_on_demand = 0;

  ETERM *reply;
  int rc;

  rc = tsdb_goto_epoch(&state->db, epoch, create_if_needed, growable,
                       load_page_on_demand);
  if (rc) {
    reply = erl_format("{error,{tsdb_goto_epoch,~i}}", rc);
  } else {
    reply = erl_format("ok");
  }

  write_term(reply, state);

  erl_free_term(reply);
}

static int check_vals_length(int vals_length, state *state) {
  if (vals_length > state->db.num_values_per_entry) {
    ETERM *error = erl_format("{error,too_many_values}");
    write_term(error, state);
    erl_free_term(error);
    return 1;
  } else {
    return 0;
  }
}

static void handle_set(ETERM *term, state *state) {
  if (check_db_opened(state)) {
    return;
  }

  ETERM *key_arg = erl_element(2, term);
  ETERM *vals_arg = erl_element(3, term);

  if (check_vals_length(erl_length(vals_arg), state)) {
      return;
  }

  char *key = erl_iolist_to_string(key_arg);
  int vals_per_entry = state->db.num_values_per_entry;
  tsdb_value *vals = erl_malloc(vals_per_entry * sizeof(tsdb_value));
  int i; ETERM *val;
  for (i = 0; i < vals_per_entry; i++) {
    val = erl_hd(vals_arg);
    if (val) {
      vals[i] = ERL_INT_UVALUE(val);
      vals_arg = erl_tl(vals_arg);
    } else {
      vals[i] = 0;
    }
  }

  ETERM *reply;
  int rc;

  rc = tsdb_set(&state->db, key, vals);
  if (rc == -2) {
    reply = erl_format("{error,missing_epoch}");
  } else if (rc) {
    reply = erl_format("{error,{tsdb_set,~i}}", rc);
  } else {
    reply = erl_format("ok");
  }

  write_term(reply, state);

  erl_free(key);
  erl_free(vals);
  erl_free_term(reply);
}

static void handle_get(ETERM *term, state *state) {
  if (check_db_opened(state)) {
    return;
  }

  ETERM *key_arg = erl_element(2, term);

  char *key = erl_iolist_to_string(key_arg);
  int vals_per_entry = state->db.num_values_per_entry;
  tsdb_value *vals;

  ETERM *reply;
  ETERM **reply_val_array;
  ETERM *reply_val_list;
  int rc, i;

  rc = tsdb_get(&state->db, key, &vals);
  if (rc >= 0) {
    reply_val_array = erl_malloc(vals_per_entry);
    for (i = 0; i < vals_per_entry; i++) {
      reply_val_array[i] = erl_mk_int(vals[i]);
    }
    reply_val_list = erl_mk_list(reply_val_array, vals_per_entry);
    reply = erl_format("{ok,~w}", reply_val_list);
  } else if (rc == -1) {
    reply = erl_format("{error,not_found}");
  } else if (rc == -2) {
    reply = erl_format("{error,missing_epoch}");
  } else {
    reply = erl_format("{error,{tsdb_get,~i}}", rc);
  }

  write_term(reply, state);

  erl_free(key);
  erl_free_compound(reply);
}

static void handle_flush(ETERM *pattern, state *state) {
  tsdb_flush(&state->db);
  ETERM *reply = erl_format("ok");
  write_term(reply, state);
  erl_free_term(reply);
}

static void handle_cmd(byte *buf, state *state, int handler_count,
                       cmd_handler *handlers) {
  ETERM *term;
  int i, handled = 0;

  term = erl_decode(buf);

  for (i = 0; !handled && i < handler_count; i++) {
    if (erl_match(handlers[i].pattern, term)) {
      handlers[i].handler(term, state);
      handled = 1;
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
  db_init(&state.db);
  trace_init();

  int cmd_len;
  byte cmd_buf[CMD_BUF_SIZE];

  int HANDLER_COUNT = 8;
  cmd_handler handlers[HANDLER_COUNT];
  handlers[0].pattern = erl_format("ping");
  handlers[0].handler = &handle_ping;
  handlers[1].pattern = erl_format("info");
  handlers[1].handler = &handle_info;
  handlers[2].pattern = erl_format("{open,_,_,_,_}");
  handlers[2].handler = &handle_open;
  handlers[3].pattern = erl_format("close");
  handlers[3].handler = &handle_close;
  handlers[4].pattern = erl_format("{goto,_}");
  handlers[4].handler = &handle_goto;
  handlers[5].pattern = erl_format("{set,_,_}");
  handlers[5].handler = &handle_set;
  handlers[6].pattern = erl_format("{get,_}");
  handlers[6].handler = &handle_get;
  handlers[7].pattern = erl_format("flush");
  handlers[7].handler = &handle_flush;

  while (1) {
    cmd_len = read_cmd(CMD_BUF_SIZE, cmd_buf);
    if (cmd_len == 0) {
      exit(EXIT_OK);
    } else if (cmd_len < 0) {
      exit(EXIT_PORT_READ_ERROR);
    } else {
      handle_cmd(cmd_buf, &state, HANDLER_COUNT, handlers);
    }
  }

  return 0;
}
