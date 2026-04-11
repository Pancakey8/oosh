#include "wrapper.h"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

struct Value jobs(struct ProgramState *state) {
  struct Value *vals = NULL;
  for (struct Job *job = job_begin(state); job; job = job->next) {
    struct Value jobid = value_num(job->jobid);
    arrpush(vals, jobid);
  }
  struct Value arr = value_array(vals, arrlenu(vals));
  arrfree(vals);
  return arr;
}

struct Value dummy(struct ProgramState *state) {
  return value_num(10);
}

static struct FuncEntry entries[] = {
  {.name = "Jobs", .function = jobs},
  {.name = "Dummy", .function = dummy}
};

__attribute__((visibility("default")))
struct ModuleEntry oosh_entry(void) {
  static struct ModuleEntry entry = {
    .functions = entries,
    .function_count = sizeof(entries) / sizeof(struct FuncEntry)
  };

  return entry;
}
