#include "wrapper.h"
#include <signal.h>
#include <stdio.h>
#include <sys/wait.h>
#include <unistd.h>
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

struct Value foreground(struct ProgramState *state) {
  struct Value job_arg = program_var(state, "0");
  if (job_arg.kind != ValNum)
    assert(false && "TODO: Error handling, foreground expected number");
  size_t job_id = job_arg.data.num;

  struct Job *job = job_find(state, job_id);
  if (!job)
    assert(false && "TODO: Error handling, foreground can't find job");

  signal(SIGTTOU, SIG_IGN);
  signal(SIGTTIN, SIG_IGN);

  kill(-job->pgid, SIGCONT);
  tcsetpgrp(STDIN_FILENO, job->pgid);
  int status;
  waitpid(-job->pgid, &status, WUNTRACED);

  if (WIFSTOPPED(status)) {
    printf("OOSH: Job %zu suspended\n", job_id);
  } else {
    printf("OOSH: Job %zu terminated\n", job_id);
    free(job_pop(state, job_id));
    job = NULL;
  }

  tcsetpgrp(STDIN_FILENO, getpgrp());
  signal(SIGTTOU, SIG_DFL);
  signal(SIGTTIN, SIG_DFL);

  return value_void();
}

static struct FuncEntry entries[] = {{.name = "Jobs", .function = jobs},
                                     {.name = "Fg", .function = foreground}};

__attribute__((visibility("default"))) struct ModuleEntry oosh_entry(void) {
  static struct ModuleEntry entry = {
      .functions = entries,
      .function_count = sizeof(entries) / sizeof(struct FuncEntry)};

  return entry;
}
