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

void print_instruction(struct IRInstr const *instr) {
  switch (instr->kind) {
  case PushNum: {
    printf("PushNum %lf\n", instr->data.num);
  } break;

  case PushTemplate: {
    printf("PushTemplate TODO\n");
  } break;

  case PushPath: {
    printf("PushPath TODO\n");
  } break;

  case PushFn: {
    printf("PushFn %s\n", instr->data.fn);
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
