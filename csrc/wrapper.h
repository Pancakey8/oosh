#include <stddef.h>
#include <sys/types.h>
#include <stdbool.h>

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

enum Operator { OpPlus, OpMinus, OpAst, OpSlash, OpIndex };

struct IRInstr {
  enum IRInstrKind {
    PushNum,
    PushTemplate,
    PushPath,
    PushFn,
    PushArray,
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
    size_t push_array;
    enum Operator apply_op;
  } data;
};

struct IRInstr instr_copy(struct IRInstr instr);
void instr_free(struct IRInstr instr);

// Use for stb fat pointers
#define stb(T) T

struct ThunkValue;

struct Value;
struct ProgramState;

typedef struct Value (*internal_function_type)(struct ProgramState *state);

struct Value {
  enum ValueKind { ValNum, ValStr, ValFunc, ValArray, ValVoid, ValThunk } kind;

  union {
    double num;
    char *stb(str);
    struct Value *stb(array);
    struct {
      bool is_internal;
      union {
        struct {
          struct IRInstr *stb(instrs);
          struct VarKV *stb(captures);
        };
        internal_function_type internal;
      };
    } func;
    struct ThunkValue *thunk;
  } data;

  size_t *rc;
};

struct Value value_num(double n);
struct Value value_str(char const *str);
struct Value value_func(struct IRInstr const *instrs, size_t instrs_length);
struct Value value_funcint(internal_function_type fptr);
struct Value value_array(struct Value *vals, size_t vals_length);
struct Value value_void(void);

struct ProgramState;

struct Value value_shallowcpy(struct Value val);
struct Value value_own(struct Value val);
void value_drop(struct ProgramState *state, struct Value val);

char *stb(value_as_string)(struct ProgramState *state, struct Value val);

struct ProgramState *init_program(void);
struct Value program_var(struct ProgramState *state, char *name);
void program_free(struct ProgramState *state);
void program_import(struct ProgramState *state, char *so_path);

struct Job {
  pid_t pgid;
  size_t jobid;
  struct Job *next;
};

size_t job_add(struct ProgramState *state, pid_t pgid);
struct Job *job_pop(struct ProgramState *state, size_t jobid);
struct Job *job_find(struct ProgramState *state, size_t jobid);
struct Job *job_begin(struct ProgramState *state);

struct FuncEntry {
  char const *name;
  internal_function_type function;
};

struct ModuleEntry {
  struct FuncEntry *functions;
  size_t function_count;
};

// Module entrypoint is called `oosh_entry`
typedef struct ModuleEntry(*module_entry_type)(void);

void eval_program(struct ProgramState *state, struct IRInstr *instrs,
                  size_t instrs_length);
