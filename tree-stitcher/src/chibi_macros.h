#include <chibi/eval.h>

sexp_sint_t _sexp_unbox_fixnum(sexp s);
sexp _sexp_car(sexp s);
sexp _sexp_cdr(sexp s);
long _sexp_length_unboxed(sexp s);
char* _sexp_string_data(sexp s);
unsigned _sexp_string_size(sexp s);
sexp _sexp_symbol_to_string(sexp ctx, sexp s);
void _sexp_debug(sexp ctx, const char* message, sexp s);
sexp _sexp_eval(sexp ctx, sexp s, sexp env);
sexp _sexp_context_env(sexp ctx);
int _sexp_pairp(sexp p);
int _sexp_symbolp(sexp s);
int _sexp_nullp(sexp s);
int _sexp_exceptionp(sexp s);
int _sexp_stringp(sexp s);
sexp _sexp_cons(sexp ctx, sexp a, sexp b);
sexp _sexp_nreverse(sexp ctx, sexp ls);
sexp _sexp_append2(sexp ctx, sexp a, sexp b);

// non-macro translation helpers
void _set_sexp_car(sexp ls, sexp in_car);

