#include "chibi_macros.h"

// a lot of the macros and even the sexp struct act up in zig's cImports, so here are manual bindings
sexp_sint_t _sexp_unbox_fixnum(sexp s) { return sexp_unbox_fixnum(s); }
sexp _sexp_car(sexp s) { return sexp_car(s); }
sexp _sexp_cdr(sexp s) { return sexp_cdr(s); }
long _sexp_length_unboxed(sexp s) { return sexp_length_unboxed(s); }
char* _sexp_string_data(sexp s) { return sexp_string_data(s); }
sexp _sexp_symbol_to_string(sexp ctx, sexp s) { return sexp_symbol_to_string(ctx, s); }
void _sexp_debug(sexp ctx, const char* message, sexp s) { sexp_debug(ctx, message, s); }
sexp _sexp_eval(sexp ctx, sexp s, sexp env) { return sexp_eval(ctx, s, env); }
sexp _sexp_context_env(sexp ctx) { return sexp_context_env(ctx); }
int _sexp_pairp(sexp s) { return sexp_pairp(s); }
int _sexp_symbolp(sexp s) { return sexp_symbolp(s); }
int _sexp_nullp(sexp s) { return sexp_nullp(s); }
sexp _sexp_cons(sexp ctx, sexp a, sexp b) { return sexp_cons(ctx, a, b); }

