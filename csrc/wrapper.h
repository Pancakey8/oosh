#include <stdio.h>
#include <stdint.h>

struct Literal;

enum StrContentKind {
  Exact = 0,
  Subst = 1
};

struct StrContent {
  uint8_t kind;
  union {
    char *exact;
    struct Literal *subst;
  } data;
};

enum PathContentKind {
  ExactPath = 0,
  Glob = 1,
  GlobRec = 2,
  StringPath = 3
};

struct PathContent {
  uint8_t kind;
  union {
    char *exact;
    struct { struct StrContent *contents; size_t contents_len } str;
  } data; // TODO
};

enum LitKind {
  NumLit = 0,
  StrLit = 1,
  VarLit = 2,
  PathLit = 3
};

struct Literal {
  uint8_t kind;
  union {
    struct NumLitData { char *whole, *frac; } num;
    struct StrLitData { struct StrContent *contents; size_t contents_len; } str;
    char *var;
  } data;
};

void stringifyLiteral(struct Literal const *lit);
