#include "wrapper.h"
#include <stdio.h>

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

void stringify_path(struct PathPart const *part, char *out,
                   size_t out_size) {
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

void eval_program(struct IRInstr *instrs, size_t instrs_length) {
  for (size_t i = 0; i < instrs_length; ++i) {
    print_instruction(&instrs[i]);
  }
}
