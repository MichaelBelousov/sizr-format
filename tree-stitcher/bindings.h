#include <tree_sitter/api.h>

// TODO: use -femit-h instead
const char* my_test(int);

struct query_match {
  TSQueryMatch match;
} query_match;

struct query_match** exec_query(const char* query, const char** paths);

