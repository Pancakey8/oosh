#include <stddef.h>

struct TemplatePart {
  enum TemplatePartKind { TempExact, TempVar } kind;

  union {
    char *exact;
    char *var;
  } data;
};

struct TemplatePart tmp_copy(struct TemplatePart part);
void tmp_free(struct TemplatePart part);

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

struct PathPart path_copy(struct PathPart part);
void path_free(struct PathPart part);

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
    PipeTo,
    Drop
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

struct IRInstr instr_copy(struct IRInstr instr);
void instr_free(struct IRInstr instr);

// Use for stb fat pointers
#define stb(T) T

struct Value;

struct Value value_num(double n);
struct Value value_str(char const *str);
struct Value value_func(struct IRInstr const *instrs, size_t instrs_length);
struct Value value_void(void);

struct Value value_shallowcpy(struct Value val);
struct Value value_own(struct Value val);
void value_drop(struct Value val);

char *stb(value_as_string)(struct Value val);

struct ProgramState;

struct ProgramState *init_program(void);
void program_free(struct ProgramState *state);

void eval_program(struct ProgramState *state, struct IRInstr *instrs,
                  size_t instrs_length);
