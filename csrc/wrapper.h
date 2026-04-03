#include <stddef.h>

struct TemplatePart {
  enum TemplatePartKind { TempExact, TempVar } kind;

  union {
    char *exact;
    char *var;
  } data;
};

struct PathPart {
  enum PathPartKind { PathExact, PathGlob, PathRecGlob, PathTmp } kind;

  union {
    char *exact;
    struct {
      struct TemplatePart *parts;
      size_t parts_length;
    } tmp;
  } data;
};

enum Operator { OpPlus, OpMinus, OpAst, OpSlash };

struct IRInstr {
  enum IRInstrKind {
    PushNum,
    PushTemplate,
    PushPath,
    PushFn,
    LoadVar,
    StoreVar,
    DefineVar,
    CallCommand,
    CallFunction,
    ApplyOp,
    PipeTo
  } kind;

  union {
    double num;
    struct {
      struct TemplatePart *parts;
      size_t parts_length;
    } tmp;
    struct {
      struct PathPart *parts;
      size_t parts_length;
    } path;
    struct {
      struct IRInstr *instrs;
      size_t instrs_length;
    } fn;
    char *load;
    char *store;
    char *define;
    size_t call_cmd;
    size_t call_fn;
    enum Operator apply_op;
  } data;
};

struct ProgramState;

struct ProgramState *init_program();

void eval_program(struct ProgramState *state, struct IRInstr *instrs,
                  size_t instrs_length);
