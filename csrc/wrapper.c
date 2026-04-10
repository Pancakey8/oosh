#include "wrapper.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

struct TemplatePart tmp_copy(struct TemplatePart part) {
  struct TemplatePart copy = part;

  switch (part.kind) {
  case TempExact: {
    copy.data.exact = strdup(part.data.exact);
  } break;
  case TempVar: {
    copy.data.var = strdup(part.data.var);
  } break;
  }

  return copy;
}

void tmp_free(struct TemplatePart part) {
  switch (part.kind) {
  case TempExact:
    free(part.data.exact);
    break;
  case TempVar:
    free(part.data.var);
    break;
  }
}

struct PathPart path_copy(struct PathPart part) {
  struct PathPart copy = part;

  switch (part.kind) {
  case PathExact: {
    copy.data.exact = strdup(part.data.exact);
  } break;
  case PathTmp: {
    copy.data.tmp.parts =
        calloc(copy.data.tmp.parts_length, sizeof(struct TemplatePart));
    for (size_t i = 0; i < copy.data.tmp.parts_length; ++i)
      copy.data.tmp.parts[i] = tmp_copy(part.data.tmp.parts[i]);
  } break;

  case PathGlob:
  case PathRecGlob:
    break;
  }

  return copy;
}

void path_free(struct PathPart part) {
  switch (part.kind) {

  case PathExact:
    free(part.data.exact);
    break;
  case PathTmp: {
    for (size_t i = 0; i < part.data.tmp.parts_length; ++i)
      tmp_free(part.data.tmp.parts[i]);
    free(part.data.tmp.parts);
  } break;
  case PathGlob:
  case PathRecGlob:
    break;
  }
}

struct IRInstr instr_copy(struct IRInstr instr) {
  struct IRInstr copy = instr;

  switch (instr.kind) {
  case PushTemplate: {
    copy.data.tmp.parts =
        calloc(copy.data.tmp.parts_length, sizeof(struct TemplatePart));
    for (size_t i = 0; i < copy.data.tmp.parts_length; ++i)
      copy.data.tmp.parts[i] = tmp_copy(instr.data.tmp.parts[i]);
  } break;
  case PushPath: {
    copy.data.path.parts =
        calloc(copy.data.path.parts_length, sizeof(struct PathPart));
    for (size_t i = 0; i < copy.data.tmp.parts_length; ++i)
      copy.data.path.parts[i] = path_copy(instr.data.path.parts[i]);
  } break;
  case PushFn: {
    copy.data.fn.instrs =
        calloc(copy.data.fn.instrs_length, sizeof(struct IRInstr));
    for (size_t i = 0; i < copy.data.fn.instrs_length; ++i)
      copy.data.fn.instrs[i] = instr_copy(instr.data.fn.instrs[i]);
  } break;
  case LoadVar: {
    copy.data.load = strdup(instr.data.load);
  } break;
  case StoreVar: {
    copy.data.store = strdup(instr.data.store);
  } break;
  case DefineVar: {
    copy.data.define = strdup(instr.data.define);
  } break;

  case PushNum:
  case PushArray:
  case CallCommand:
  case CallFunction:
  case ApplyOp:
  case PipeTo:
  case Drop:
    break;
  }

  return copy;
}

void instr_free(struct IRInstr instr) {
  switch (instr.kind) {
  case PushTemplate: {
    for (size_t i = 0; i < instr.data.tmp.parts_length; ++i)
      tmp_free(instr.data.tmp.parts[i]);
    free(instr.data.tmp.parts);
  } break;
  case PushPath: {
    for (size_t i = 0; i < instr.data.path.parts_length; ++i)
      path_free(instr.data.path.parts[i]);
    free(instr.data.path.parts);
  } break;
  case PushFn: {
    for (size_t i = 0; i < instr.data.fn.instrs_length; ++i)
      instr_free(instr.data.fn.instrs[i]);
    free(instr.data.fn.instrs);
  } break;
  case LoadVar:
    free(instr.data.load);
    break;
  case StoreVar:
    free(instr.data.store);
    break;
  case DefineVar:
    free(instr.data.define);
    break;

  case PushNum:
  case PushArray:
  case CallCommand:
  case CallFunction:
  case ApplyOp:
  case PipeTo:
  case Drop:
    break;
  }
}

char *stb(resolve_path)(const char *name) {
  if (!name)
    return NULL;

  char *result = NULL;
  char *path_env = getenv("PATH");

  if (strchr(name, '/') != NULL) { // Has slash
    if (access(name, X_OK) == 0) { // Executable
      struct stat statbuf;
      // Regular file / not directory
      if (stat(name, &statbuf) == 0 && S_ISREG(statbuf.st_mode)) {
        for (int i = 0; i <= strlen(name); i++) {
          arrpush(result, name[i]);
        }
        return result;
      }
    }
    return NULL;
  }

  if (!path_env)
    return NULL;

  char *path_copy = strdup(path_env);
  char *save;
  char *dir = strtok_r(path_copy, ":", &save);
  char buf[1024];

  while (dir != NULL) {
    snprintf(buf, sizeof(buf), "%s/%s", dir, name); // In path

    if (access(buf, X_OK) == 0) { // Executable
      struct stat statbuf;
      // Regular file / not directory
      if (stat(buf, &statbuf) == 0 && S_ISREG(statbuf.st_mode)) {
        for (int i = 0; i <= strlen(buf); i++) {
          arrpush(result, buf[i]);
        }
        free(path_copy);
        return result;
      }
    }
    dir = strtok_r(NULL, ":", &save);
  }

  free(path_copy);
  return NULL;
}

struct VarKV {
  char *key;
  struct Variable *value;
};

struct Value {
  enum ValueKind { ValNum, ValStr, ValFunc, ValArray, ValVoid, ValThunk } kind;

  union {
    double num;
    char *stb(str);
    struct Value *stb(array);
    struct {
      struct IRInstr *stb(instrs);
      struct VarKV *stb(captures);
    } func;
    struct ThunkValue *thunk;
  } data;

  size_t *rc;
};

struct ThunkValue {
  enum ThunkValueKind { ThunkProc, ThunkFunc, ThunkVal, ThunkPipe } kind;

  union {
    struct {
      char *stb(name);
      char *stb() * stb(args);
    } proc;

    struct {
      struct Value callee;
      struct ProgramState *state;
      size_t pipe_to;
    } func;

    struct Value val;

    struct {
      struct Value *from, *to;
    } pipe;
  } data;
};

struct Variable {
  struct Value value;
  size_t rc;
};

struct Variable *var_from_value(struct Value *val) {
  struct Variable *var = calloc(1, sizeof(struct Variable));
  var->value = *val;
  var->rc = 1;
  *val = (struct Value){0};
  return var;
}

struct Variable *var_keep(struct Variable *var) {
  var->rc++;
  return var;
}

void var_drop(struct Variable *var) {
  if (var->rc > 1) {
    --var->rc;
    return;
  }

  value_drop(var->value);
  free(var);
}

struct Value value_num(double n) {
  return (struct Value){.kind = ValNum, .data.num = n, .rc = NULL};
}

struct Value value_str(char const *str) {
  struct Value val = (struct Value){
      .kind = ValStr, .data.str = NULL, .rc = malloc(sizeof(size_t))};
  *val.rc = 1;
  size_t len = strlen(str);
  arrsetlen(val.data.str, len + 1);
  memcpy(val.data.str, str, len + 1);
  return val;
}

struct Value value_func(struct IRInstr const *instrs, size_t instrs_length) {
  struct Value val =
      (struct Value){.kind = ValFunc,
                     .data.func = {.instrs = NULL, .captures = NULL},
                     .rc = malloc(sizeof(size_t))};
  *val.rc = 1;
  arrsetlen(val.data.func.instrs, instrs_length);
  for (size_t i = 0; i < instrs_length; ++i) {
    val.data.func.instrs[i] = instr_copy(instrs[i]);
  }
  return val;
}

struct Value value_array(struct Value *vals, size_t vals_length) {
  struct Value val = (struct Value){
      .kind = ValArray, .data.array = NULL, .rc = malloc(sizeof(size_t))};
  *val.rc = 1;
  arrsetlen(val.data.array, vals_length);
  for (size_t i = 0; i < vals_length; ++i)
    val.data.array[i] = value_shallowcpy(vals[i]);
  return val;
}

struct Value value_void(void) {
  return (struct Value){.kind = ValVoid, .data = {0}, .rc = NULL};
}

struct Value value_shallowcpy(struct Value val) {
  if (val.kind == ValNum || val.kind == ValVoid)
    return val;
  (*val.rc)++;
  return val;
}

struct Value value_own(struct Value val) {
  if (val.kind == ValNum || val.kind == ValVoid || val.kind == ValThunk ||
      *val.rc == 1)
    return val;
  *val.rc -= 1;

  struct Value owned =
      (struct Value){.kind = val.kind, .rc = malloc(sizeof(size_t))};
  *owned.rc = 1;

  switch (val.kind) {
  case ValStr: {
    owned.data.str = NULL;
    arrsetlen(owned.data.str, arrlenu(val.data.str));
    memcpy(owned.data.str, val.data.str, arrlenu(val.data.str));
  } break;

  case ValFunc: {
    owned.data.func.instrs = NULL;
    arrsetlen(owned.data.func.instrs, arrlenu(val.data.func.instrs));
    for (size_t i = 0; i < arrlenu(val.data.func.instrs); ++i) {
      owned.data.func.instrs[i] = instr_copy(val.data.func.instrs[i]);
    }
    owned.data.func.captures = NULL;
    for (size_t i = 0; i < shlenu(val.data.func.captures); ++i) {
      shput(owned.data.func.captures, strdup(val.data.func.captures[i].key),
            var_keep(val.data.func.captures[i].value));
    }
  } break;

  case ValArray: {
    owned.data.array = NULL;
    arrsetlen(owned.data.array, arrlenu(val.data.array));
    for (size_t i = 0; i < arrlenu(val.data.array); ++i)
      owned.data.array[i] = value_shallowcpy(val.data.array[i]);
  } break;

  case ValNum:
  case ValVoid:
  case ValThunk:
    assert(false && "Unreachable");
    break;
  }

  return owned;
}

void eval_thunk(struct ThunkValue *thunk);
struct Value value_real(struct Value *value);

void value_drop(struct Value val) {
  if (val.kind == ValVoid || val.kind == ValNum)
    return;
  if (*val.rc > 1) {
    (*val.rc)--;
    return;
  }

  free(val.rc);
  switch (val.kind) {
  case ValStr: {
    arrfree(val.data.str);
  } break;

  case ValFunc: {
    for (size_t i = 0; i < arrlenu(val.data.func.instrs); ++i)
      instr_free(val.data.func.instrs[i]);
    arrfree(val.data.func.instrs);
    for (size_t i = 0; i < shlenu(val.data.func.captures); ++i) {
      free(val.data.func.captures[i].key);
      var_drop(val.data.func.captures[i].value);
    }
    shfree(val.data.func.captures);
  } break;

  case ValArray: {
    for (size_t i = 0; i < arrlenu(val.data.array); ++i) {
      value_drop(val.data.array[i]);
    }
    arrfree(val.data.array);
  } break;

  case ValThunk: {
    eval_thunk(val.data.thunk);
    value_drop(val.data.thunk->data.val);
    free(val.data.thunk);
  } break;

  case ValNum:
  case ValVoid:
    assert(false && "Unreachable");
    break;
  }
}

char *stb(value_as_string)(struct Value val) {
  switch (val.kind) {
  case ValNum: {
    int n = snprintf(NULL, 0, "%lf", val.data.num);
    char *str = NULL;
    arrsetlen(str, n + 1);
    snprintf(str, n + 1, "%lf", val.data.num);
    return str;
  } break;
  case ValStr: {
    char *str = NULL;
    arrsetlen(str, arrlenu(val.data.str));
    memcpy(str, val.data.str, arrlenu(val.data.str));
    return str;
  } break;
  case ValFunc: {
    char *fn = "<function>";
    char *str = NULL;
    arrsetlen(str, strlen(fn) + 1);
    strcpy(str, fn);
    return str;
  } break;
  case ValArray: {
    char *str = NULL;
    arrput(str, '[');
    arrput(str, ' ');

    for (size_t i = 0; i < arrlenu(val.data.array); i++) {
      char *element_str = value_as_string(val.data.array[i]);

      if (i > 0) {
        arrput(str, ',');
        arrput(str, ' ');
      }

      size_t el_len = arrlenu(element_str) - 1;
      size_t current_len = arrlenu(str);

      arrsetlen(str, current_len + el_len);
      memcpy(str + current_len, element_str, el_len);

      arrfree(element_str);
    }

    arrput(str, ' ');
    arrput(str, ']');
    arrput(str, '\0');
    return str;
  } break;
  case ValThunk: {
    struct Value cpy = value_shallowcpy(val);
    cpy = value_real(&cpy);
    char *s = value_as_string(cpy);
    value_drop(cpy);
    return s;
  } break;
  case ValVoid: {
    char *vd = "<void>";
    char *str = NULL;
    arrsetlen(str, strlen(vd) + 1);
    strcpy(str, vd);
    return str;
  } break;
  }
}

struct ProgramState {
  struct VarKV *stb(local), *stb(global);

  // Evaluation stack
  struct Value *stb(stack);
};

struct ProgramState *init_program() {
  struct ProgramState *state = calloc(1, sizeof(typeof(*state)));
  return state;
}

void program_free(struct ProgramState *state) {
  if (!state)
    return;

  if (state->stack) {
    for (size_t i = 0; i < arrlenu(state->stack); ++i)
      value_drop(state->stack[i]);
    arrfree(state->stack);
  }

  if (state->local) {
    for (size_t i = 0; i < shlenu(state->local); ++i) {
      free(state->local[i].key);
      var_drop(state->local[i].value);
    }
    shfree(state->local);
  }

  if (state->global) {
    for (size_t i = 0; i < shlenu(state->global); ++i) {
      free(state->global[i].key);
      var_drop(state->global[i].value);
    }
    shfree(state->global);
  }

  free(state);
}

char *stb(eval_tmps)(struct ProgramState *state, struct TemplatePart *parts,
                     size_t parts_length) {
  char *res = NULL;
  arrput(res, '\0');
  for (size_t i = 0; i < parts_length; ++i) {
    switch (parts[i].kind) {
    case TempExact: {
      size_t len = arrlenu(res) - 1;
      size_t part_len = strlen(parts[i].data.exact);
      arrsetlen(res, len + part_len + 1);
      memcpy(&res[len], parts[i].data.exact, part_len + 1);
    } break;
    case TempVar: {
      struct VarKV *kv;
      if (!(kv = shgetp_null(state->local, parts[i].data.var))) {
        if (!(kv = shgetp_null(state->global, parts[i].data.var))) {
          assert(false && "TODO: Error handling, variable not defined");
        }
      }
      char *s = value_as_string(kv->value->value);
      size_t len = arrlenu(res) - 1;
      size_t s_len = arrlenu(s) - 1;
      arrsetlen(res, len + s_len + 1);
      memcpy(&res[len], s, s_len + 1);
      arrfree(s);
    } break;
    }
  }
  return res;
}

void eval_instr(struct ProgramState *state, struct IRInstr instr);

void eval_thunk(struct ThunkValue *thunk) {
  switch (thunk->kind) {
  case ThunkProc: {
    int pipefd[2];
    if (pipe(pipefd) < 0) {
      assert(false && "TODO: Error handling, pipe failed");
    }

    pid_t proc = fork();
    if (proc < 0)
      assert(false && "TODO: Error handling, fork failed");

    if (proc == 0) {
      setpgid(0, 0);

      dup2(pipefd[1], STDOUT_FILENO);
      close(pipefd[0]);
      close(pipefd[1]);

      execv(thunk->data.proc.name, thunk->data.proc.args);
    } else {
      close(pipefd[1]);

      char *stb(output) = NULL;
      char buffer[4096];
      ssize_t bytes_read;

      while ((bytes_read = read(pipefd[0], buffer, sizeof(buffer))) > 0) {
        size_t len = arrlenu(output);
        arraddnindex(output, bytes_read);
        memcpy(output + len, buffer, bytes_read);
      }

      arrpush(output, '\0');

      close(pipefd[0]);
      int status;
      waitpid(proc, &status, 0);

      struct Value str = value_str(output);
      arrfree(output);
      struct ThunkValue next = {.kind = ThunkVal, .data.val = str};

      arrfree(thunk->data.proc.name);
      for (size_t i = 0; i < arrlen(thunk->data.proc.args); ++i) {
        arrfree(thunk->data.proc.args[i]);
      }
      arrfree(thunk->data.proc.args);
      *thunk = next;
    }
  } break;
  case ThunkFunc: {
    assert(thunk->data.func.callee.kind == ValFunc &&
           "Thunk can't store non-function callee");
    for (size_t i = 0; i < arrlenu(thunk->data.func.callee.data.func.instrs);
         ++i) {
      eval_instr(thunk->data.func.state,
                 thunk->data.func.callee.data.func.instrs[i]);
    }

    assert(arrlenu(thunk->data.func.state->stack) == 1 &&
           "Function must result in one value");
    struct Value res = arrpop(thunk->data.func.state->stack);
    struct ThunkValue next = {.kind = ThunkVal, .data.val = res};

    program_free(thunk->data.func.state);
    value_drop(thunk->data.func.callee);

    *thunk = next;
  } break;
  case ThunkVal: {
    // Already evaluated
  } break;
  case ThunkPipe: {
    switch (thunk->data.pipe.to->data.thunk->kind) {
    case ThunkProc: {
      assert(false && "TODO: Handle process piping");
    } break;
    case ThunkFunc: {
      struct Value from = value_real(thunk->data.pipe.from);

      int arg_name_len = snprintf(
          NULL, 0, "%zu", thunk->data.pipe.to->data.thunk->data.func.pipe_to);
      char *arg_name = malloc(arg_name_len + 1);
      snprintf(arg_name, arg_name_len + 1, "%zu",
               thunk->data.pipe.to->data.thunk->data.func.pipe_to);
      shput(thunk->data.pipe.to->data.thunk->data.func.state->local, arg_name,
            var_from_value(&from));

      struct Value val = value_real(thunk->data.pipe.to);
      free(thunk->data.pipe.from);
      free(thunk->data.pipe.to);

      struct ThunkValue next = {.kind = ThunkVal, .data.val = val};
      *thunk = next;
    } break;
    case ThunkVal:
    case ThunkPipe:
      assert(false && "TODO: Error handling, can't pipe to this");
      break;
    }
  } break;
  }
}

struct Value value_real(struct Value *value) {
  struct Value mov = *value;
  *value = (struct Value){0};
  if (mov.kind != ValThunk)
    return mov;
  eval_thunk(mov.data.thunk);
  struct Value res = value_shallowcpy(mov.data.thunk->data.val);
  value_drop(mov);
  return res;
}

void eval_instr(struct ProgramState *state, struct IRInstr instr) {
  switch (instr.kind) {
  case PushNum: {
    arrpush(state->stack, value_num(instr.data.num));
  } break;
  case PushTemplate: {
    char *result =
        eval_tmps(state, instr.data.tmp.parts, instr.data.tmp.parts_length);
    arrpush(state->stack, value_str(result));
    arrfree(result);
  } break;
  case PushPath: {
    assert(false && "TODO: Paths");
  } break;
  case PushArray: {
    size_t n = instr.data.push_array;
    assert(arrlenu(state->stack) >= n);
    struct Value *vals = calloc(n, sizeof(struct Value));
    if (n != 0) {
      for (size_t i = n - 1;; --i) {
        struct Value v = arrpop(state->stack);
        vals[i] = value_real(&v);
        if (i == 0)
          break;
      }
    }
    arrpush(state->stack, value_array(vals, n));
    for (size_t i = 0; i < n; ++i)
      value_drop(vals[i]);
    free(vals);
  } break;
  case PushFn: { // TODO: This might be fragile
    struct Value fn =
        value_func(instr.data.fn.instrs, instr.data.fn.instrs_length);
    for (size_t i = 0; i < shlenu(state->local); ++i) {
      struct VarKV kv = state->local[i];
      shput(fn.data.func.captures, strdup(kv.key), var_keep(kv.value));
    }
    for (size_t i = 0; i < shlenu(state->global); ++i) {
      struct VarKV kv = state->global[i];
      if (shgeti(fn.data.func.captures, kv.key) < 0)
        shput(fn.data.func.captures, strdup(kv.key), var_keep(kv.value));
    }
    arrpush(state->stack, fn);
  } break;
  case LoadVar: {
    struct VarKV *kv;
    if (!(kv = shgetp_null(state->local, instr.data.load))) {
      if (!(kv = shgetp_null(state->global, instr.data.load))) {
        assert(false && "TODO: Error handling, variable not defined");
      }
    }
    struct Value v = value_shallowcpy(kv->value->value);
    arrpush(state->stack, v);
  } break;
  case StoreVar: {
    assert(arrlenu(state->stack) >= 1);
    struct VarKV *kv;
    if (!(kv = shgetp_null(state->local, instr.data.store))) {
      if (!(kv = shgetp_null(state->global, instr.data.store))) {
        assert(false && "TODO: Error handling, variable not defined");
      }
    }
    value_drop(kv->value->value);
    struct Value v = arrpop(state->stack);
    kv->value->value = value_real(&v);
    arrpush(state->stack, value_void());
  } break;
  case DefineVar: {
    assert(arrlenu(state->stack) >= 1);
    if (shgeti(state->local, instr.data.define) >= 0) {
      assert(false &&
             "TODO: Error handling, attempt to redefine existing variable");
    }
    struct Value v = arrpop(state->stack);
    v = value_real(&v);
    shput(state->local, strdup(instr.data.define), var_from_value(&v));
    arrpush(state->stack, value_void());
  } break;
  case Drop: {
    struct Value v = arrpop(state->stack);
    value_drop(v);
  } break;
  case CallFunction: {
    assert(arrlenu(state->stack) >= instr.data.call_fn + 1);
    struct Value callee = arrpop(state->stack);
    callee = value_real(&callee);

    if (callee.kind != ValFunc) {
      assert(false && "TODO: Error handling, calling non-function value");
    }

    struct ProgramState *subroutine = init_program();

    // TODO: This might be fragile
    for (size_t i = 0; i < shlenu(callee.data.func.captures); ++i) {
      struct VarKV kv = callee.data.func.captures[i];
      shput(subroutine->global, strdup(kv.key), var_keep(kv.value));
    }

    if (instr.data.call_fn != 0) {
      for (size_t i = instr.data.call_fn - 1;; --i) {
        int arg_name_len = snprintf(NULL, 0, "%zu", i);
        char *arg_name = malloc(arg_name_len + 1);
        snprintf(arg_name, arg_name_len + 1, "%zu", i);
        struct Value arg = arrpop(state->stack);
        arg = value_real(&arg);
        shput(subroutine->local, arg_name, var_from_value(&arg));
        if (i == 0)
          break;
      }
    }

    struct Value val = {.kind = ValThunk,
                        .data.thunk = calloc(1, sizeof(struct ThunkValue)),
                        .rc = malloc(sizeof(size_t))};
    *val.rc = 1;
    val.data.thunk->kind = ThunkFunc;
    val.data.thunk->data.func.callee = callee;
    val.data.thunk->data.func.state = subroutine;
    val.data.thunk->data.func.pipe_to = instr.data.call_fn;
    arrpush(state->stack, val);
  } break;
  case PipeTo: {
    assert(arrlenu(state->stack) >= 2);
    struct Value rhs = arrpop(state->stack);
    struct Value lhs = arrpop(state->stack);

    if (rhs.kind != ValThunk)
      assert(false && "TODO: Error handling, piping into non-thunk");

    if (lhs.kind != ValThunk) {
      struct Value thunk =
          (struct Value){.kind = ValThunk,
                         .data.thunk = calloc(1, sizeof(struct ThunkValue)),
                         .rc = malloc(sizeof(size_t))};
      *thunk.rc = 1;
      thunk.data.thunk->kind = ThunkVal;
      thunk.data.thunk->data.val = lhs;
      lhs = thunk;
    }

    struct Value pipe =
        (struct Value){.kind = ValThunk,
                       .data.thunk = calloc(1, sizeof(struct ThunkValue)),
                       .rc = malloc(sizeof(size_t))};
    *pipe.rc = 1;
    pipe.data.thunk->kind = ThunkPipe;
    pipe.data.thunk->data.pipe.from = malloc(sizeof(struct Value));
    *pipe.data.thunk->data.pipe.from = lhs;
    pipe.data.thunk->data.pipe.to = malloc(sizeof(struct Value));
    *pipe.data.thunk->data.pipe.to = rhs;

    arrpush(state->stack, pipe);
  } break;
  case CallCommand: {
    assert(arrlenu(state->stack) >= instr.data.call_cmd + 1);
    struct Value command = arrpop(state->stack);

    struct ThunkValue cmd = {.kind = ThunkProc, .data.proc = {0}};

    char *name = value_as_string(command);
    cmd.data.proc.name = resolve_path(name);
    arrfree(name);

    if (!cmd.data.proc.name)
      assert(false && "TODO: Error handling, invalid path");

    cmd.data.proc.args = NULL;
    arrsetlen(cmd.data.proc.args, instr.data.call_cmd + 2);
    for (size_t i = instr.data.call_cmd; i > 0; --i) {
      struct Value arg = arrpop(state->stack);
      cmd.data.proc.args[i] = value_as_string(arg);
      value_drop(arg);
    }
    cmd.data.proc.args[0] = NULL;
    arrsetlen(cmd.data.proc.args[0], arrlenu(cmd.data.proc.name));
    memcpy(cmd.data.proc.args[0], cmd.data.proc.name,
           arrlenu(cmd.data.proc.name));
    cmd.data.proc.args[instr.data.call_cmd + 1] = NULL;

    struct Value proc = {.kind = ValThunk,
                         .data.thunk = calloc(1, sizeof(struct ThunkValue)),
                         .rc = malloc(sizeof(size_t))};
    *proc.rc = 1;
    *proc.data.thunk = cmd;

    arrput(state->stack, proc);

    value_drop(command);
  } break;
  case ApplyOp:
    break;
  }
}

void eval_program(struct ProgramState *state, struct IRInstr *instrs,
                  size_t instrs_length) {
  for (size_t i = 0; i < arrlenu(state->stack); ++i) {
    value_drop(state->stack[i]);
  }
  arrfree(state->stack);
  state->stack = NULL; // Clearing stack to be safe

  for (size_t i = 0; i < instrs_length; ++i)
    eval_instr(state, instrs[i]);

  assert(arrlenu(state->stack) == 1 &&
         "Program must result in exactly 1 value");

  struct Value v = arrpop(state->stack);
  char *s = value_as_string(v);
  printf(">> %s\n", s);
  value_drop(v);
  arrfree(s);

  for (size_t i = 0; i < instrs_length; ++i)
    instr_free(instrs[i]);
  free(instrs);
}
