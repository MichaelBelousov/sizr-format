#include <chibi/eval.h>

sexp_sint_t _sexp_unbox_fixnum(sexp s);
sexp _sexp_car(sexp s);
long _sexp_length_unboxed(sexp s);
char* _sexp_string_data(sexp s);
sexp _sexp_symbol_to_string(sexp ctx, sexp s);
void _sexp_debug(sexp ctx, const char* message, sexp s);

