#include <tree_sitter/api.h>
// TODO: use -femit-h instead, but it's not available yet
//
struct query_match {
  TSQueryMatch match;
} query_match;

struct query_match** exec_query(const char* query, const char** paths);

