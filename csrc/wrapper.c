#include "wrapper.h"
#include <stdio.h>
#include <string.h>

void stringifyStrContent(struct StrContent const *content) {
  switch ((enum StrContentKind)content->kind) {
  case Exact: {
    printf("%s", content->data.exact);
  } break;

  case Subst: {
    printf("$%s", content->data.subst->data.var);
  } break;
  }
}

void stringifyLiteral(struct Literal const *lit) {
  switch ((enum LitKind)lit->kind) {
  case NumLit: {
    if (strlen(lit->data.num.frac) == 0) {
      printf("%s\n", lit->data.num.whole);
    } else {
      printf("%s.%s\n", lit->data.num.whole, lit->data.num.frac);
    }
  } break;

  case StrLit: {
    putchar('"');
    for (size_t i = 0; i < lit->data.str.contents_len; ++i) {
      stringifyStrContent(&lit->data.str.contents[i]);
    }
    puts("\"\n");
  } break;

  case VarLit: {
    printf("$%s\n", lit->data.var);
  } break;

  case PathLit:
    break;
  }
}
