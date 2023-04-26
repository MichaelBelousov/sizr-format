#include <tree_sitter/api.h>

// TODO: use -femit-h instead, but it's not available yet
const TSQueryMatch** exec_query(const char* query, const char** paths);

