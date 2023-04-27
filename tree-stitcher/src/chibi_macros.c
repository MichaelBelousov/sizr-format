#include "chibi_macros.h"

// a lot of the macros and even the sexp struct act up in zig's cImports, so here are manual bindings
sexp_sint_t _sexp_unbox_fixnum(sexp s) { return sexp_unbox_fixnum(s); }
sexp _sexp_car(sexp ctx, sexp s) { return _sexp_car(ctx, s); }
long _sexp_length_unboxed(sexp s) { return sexp_length_unboxed(s); }
char* _sexp_string_data(sexp s) { return sexp_string_data(s); }
sexp _sexp_symbol_to_string(sexp ctx, sexp s) { return sexp_symbol_to_string(ctx, s); }
