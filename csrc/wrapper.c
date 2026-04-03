#include "wrapper.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

char *stringify_op(enum Operator op) {
  switch (op) {
  case OpPlus:
    return "+";
  case OpMinus:
    return "-";
  case OpAst:
    return "*";
  case OpSlash:
    return "/";
  }
}

void stringify_tmp(struct TemplatePart const *part, char *out,
                   size_t out_size) {
  switch (part->kind) {
  case TempExact: {
    snprintf(out, out_size, "Exact '%s'", part->data.exact);
  } break;
  case TempVar: {
    snprintf(out, out_size, "Var %s", part->data.var);
  } break;
  }
}

void stringify_path(struct PathPart const *part, char *out, size_t out_size) {
  switch (part->kind) {
  case PathExact: {
    snprintf(out, out_size, "Exact '%s'", part->data.exact);
  } break;
  case PathGlob: {
    snprintf(out, out_size, "Glob");
  } break;
  case PathRecGlob: {
    snprintf(out, out_size, "RecGlob");
  } break;
  case PathTmp: {
    snprintf(out, out_size, "Template [%zu]", part->data.tmp.parts_length);
  } break;
  }
}

void print_instruction(struct IRInstr const *instr) {
  switch (instr->kind) {
  case PushNum: {
    printf("PushNum %lf\n", instr->data.num);
  } break;

  case PushTemplate: {
    printf("PushTemplate\n");
    char out[1024];
    for (size_t i = 0; i < instr->data.tmp.parts_length; ++i) {
      stringify_tmp(&instr->data.tmp.parts[i], out, 1024);
      printf("- %s\n", out);
    }
  } break;

  case PushPath: {
    printf("PushPath\n");
    char out[1024];
    for (size_t i = 0; i < instr->data.path.parts_length; ++i) {
      stringify_path(&instr->data.path.parts[i], out, 1024);
      printf("- %s\n", out);
    }
  } break;

  case PushFn: {
    printf("PushFn {\n");
    for (size_t i = 0; i < instr->data.fn.instrs_length; ++i) {
      print_instruction(&instr->data.fn.instrs[i]);
    }
    printf("}\n");
  } break;

  case LoadVar: {
    printf("LoadVar %s\n", instr->data.load);
  } break;

  case StoreVar: {
    printf("StoreVar %s\n", instr->data.store);
  } break;

  case DefineVar: {
    printf("DefineVar %s\n", instr->data.define);
  } break;

  case CallCommand: {
    printf("CallCommand %zu\n", instr->data.call_cmd);
  } break;

  case CallFunction: {
    printf("CallFunction %zu\n", instr->data.call_fn);
  } break;

  case ApplyOp: {
    printf("ApplyOp %s\n", stringify_op(instr->data.apply_op));
  } break;

  case PipeTo: {
    printf("PipeTo\n");
  } break;
  }
}

struct Value {
  enum ValueKind { ValNum, ValStr, ValFn } kind;

  union {
    double num;
    char *str; // TODO: Store length
    struct {
      struct IRInstr *instrs;
      size_t instrs_length;
    } fn;
  } data;
};

struct Value stringify(struct Value v) {
  switch (v.kind) {
  case ValNum: {
    int len = snprintf(NULL, 0, "%lf", v.data.num);
    char *s = malloc(len + 1);
    snprintf(s, len + 1, "%lf", v.data.num);
    return (struct Value){.kind = ValStr, .data.str = s};
  } break;
  case ValStr:
    return v;
  case ValFn:
    return (struct Value){.kind = ValFn, .data.str = strdup("<function>")};
  }
}

void typecast(struct Value *val1, struct Value *val2) {
  if (val1->kind == val2->kind)
    return;

  if (val1->kind == ValStr) {
    *val2 = stringify(*val2); // TODO: Handle old val2
    return;
  } else if (val2->kind == ValStr) {
    *val1 = stringify(*val1); // TODO: Handle old val1
    return;
  }

  if (val1->kind == ValFn || val2->kind == ValFn) {
    assert(false && "TODO: Error handling: cannot cast value to function");
  }
}

struct Value value_add(struct Value left, struct Value right) {
  typecast(&left, &right);

  if (left.kind == ValNum && right.kind == ValNum) {
    return (struct Value){.kind = ValNum,
                          .data.num = left.data.num + right.data.num};
  }

  if (left.kind == ValStr && right.kind == ValStr) {
    size_t left_len = strlen(left.data.str);
    size_t right_len = strlen(right.data.str);
    char *cat = calloc(left_len + right_len + 1, sizeof(char));
    memcpy(cat, left.data.str, left_len);
    memcpy(&cat[left_len], right.data.str, right_len);
    return (struct Value){.kind = ValStr, .data.str = cat};
  }

  if (left.kind == ValFn && right.kind == ValFn) {
    assert(false && "TODO: Error handling: cannot sum functions");
  }

  assert(false && "Unreachable due to typecast matching types");
}

struct ProgramState {
  struct Value *stack;
  struct {
    char *key;
    struct Value value;
  } *locals, *globals;
};

struct ProgramState *init_program() {
  struct ProgramState *state = calloc(1, sizeof(typeof(*state)));
  return state;
}

struct Value eval_template(struct ProgramState *state,
                           struct TemplatePart *parts, size_t parts_length) {
  char *total = malloc(1);
  size_t total_length = 0;
  for (size_t i = 0; i < parts_length; ++i) {
    switch (parts[i].kind) {
    case TempExact: {
      size_t start = total_length;
      size_t len = strlen(parts[i].data.exact);
      total_length += len;
      total = realloc(total, total_length + 1);
      memcpy(&total[start], parts[i].data.exact, len);
    } break;
    case TempVar: {
      typeof(*state->locals) *value;
      if ((value = shgetp_null(state->locals, parts[i].data.var)) == NULL) {
        if ((value = shgetp_null(state->globals, parts[i].data.var)) == NULL) {
          assert(false && "TODO: Error handling: Accessing undefined variable");
        }
      }
      struct Value v = stringify(value->value);
      size_t start = total_length;
      size_t len = strlen(v.data.str);
      total_length += len;
      total = realloc(total, total_length + 1);
      memcpy(&total[start], v.data.str, len);
    } break;
    }
  }
  total[total_length] = '\0';
  return (struct Value){.kind = ValStr, .data.str = total};
}

void eval_instr(struct ProgramState *state, struct IRInstr const *instr) {
  switch (instr->kind) {
  case PushNum: {
    struct Value val =
        (struct Value){.kind = ValNum, .data.num = instr->data.num};
    arrput(state->stack, val);
  } break;
  case DefineVar: {
    assert(arrlen(state->stack) > 0);

    if (shgetp_null(state->locals, instr->data.define)) {
      assert(false && "TODO: Error handling: Redefining existing variable");
      return;
    }

    struct Value val = arrpop(state->stack);
    shput(state->locals, instr->data.define, val);
  } break;
  case LoadVar: {
    typeof(*state->locals) *value;
    if ((value = shgetp_null(state->locals, instr->data.load)) == NULL) {
      if ((value = shgetp_null(state->globals, instr->data.load)) == NULL) {
        assert(false && "TODO: Error handling: Accessing undefined variable");
        return;
      }
    }
    arrput(state->stack, value->value);
  } break;
  case StoreVar: {
    assert(arrlen(state->stack) > 0);

    typeof(*state->locals) *value;
    if ((value = shgetp_null(state->locals, instr->data.load)) == NULL) {
      if ((value = shgetp_null(state->globals, instr->data.load)) == NULL) {
        assert(false && "TODO: Error handling: Accessing undefined variable");
        return;
      }
    }

    struct Value val = arrpop(state->stack);
    value->value = val;
    // TODO: Free old value
  } break;
  case ApplyOp: {
    assert(arrlen(state->stack) >= 2);
    // TODO: Free left/right
    struct Value right = arrpop(state->stack);
    struct Value left = arrpop(state->stack);

    switch (instr->data.apply_op) {
    case OpPlus: {
      arrput(state->stack, value_add(left, right));
    } break;
    case OpMinus:
    case OpAst:
    case OpSlash:
      assert(false && "TODO: Operator");
      break;
    }
  } break;
  case PushTemplate: {
    struct Value str = eval_template(state, instr->data.tmp.parts,
                                     instr->data.tmp.parts_length);
    arrput(state->stack, str);
  } break;
  case PushFn: {
    struct Value fn =
        (struct Value){.kind = ValFn,
                       .data.fn.instrs = instr->data.fn.instrs,
                       .data.fn.instrs_length = instr->data.fn.instrs_length};
    arrput(state->stack, fn);
  } break;
  case CallFunction: {
    assert(arrlen(state->stack) >= 1 + instr->data.call_fn);
    struct Value callee = arrpop(state->stack);
    if (callee.kind != ValFn) {
      assert(false && "TODO: Error handling: cannot call non-function");
    }

    struct ProgramState *call_scope = init_program();
    for (size_t i = 0; i < shlen(state->globals); ++i) {
      shput(call_scope->globals, state->globals[i].key, state->globals[i].value);
    }
    for (size_t i = 0; i < shlen(state->locals); ++i) {
      shput(call_scope->globals, state->locals[i].key, state->locals[i].value);
    }
    for (size_t i = instr->data.call_fn - 1; ; --i) {
      int len = snprintf(NULL, 0, "%zu", i);
      char *s = malloc(sizeof(len) + 1);
      snprintf(s, len + 1, "%zu", i);
      shput(call_scope->locals, s, arrpop(state->stack));

      if (i == 0) break;
    }

    eval_program(call_scope, callee.data.fn.instrs, callee.data.fn.instrs_length);
    if (arrlen(call_scope->stack) > 0) {
      arrput(state->stack, arrpop(call_scope->stack));
    }
    // TODO: Free call_scope
  } break;
  case PushPath:
  case CallCommand:
  case PipeTo:
    assert(false && "TODO: Instruction");
    break;
  }
}

void eval_program(struct ProgramState *state, struct IRInstr *instrs,
                  size_t instrs_length) {
  arrfree(state->stack);
  state->stack = NULL;

  /* for (size_t i = 0; i < instrs_length; ++i) { */
  /*   print_instruction(&instrs[i]); */
  /* } */

  for (size_t i = 0; i < instrs_length; ++i) {
    eval_instr(state, &instrs[i]);
  }

  printf("Final stack:\n");
  for (size_t i = 0; i < arrlen(state->stack); ++i) {
    printf("- %s\n", stringify(state->stack[i]).data.str);
  }
}
