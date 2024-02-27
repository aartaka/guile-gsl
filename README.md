# Guile GSL Bindings

This binds a minimal set of GSL functions (the ones needed for the
guile-gemma project, at least—contribute more if you feel like it) for
use in Guile.

## Build Process

It's a bit of a mess. Guile cannot find the CBLAS symbols when
building against raw GSL, that's why there is `libguilegslblas` and
Makefile. So the rough sequence of steps for building is:
- Compile `libguilegslblas` with a C compiler and GSL accessible as a
  dynamic library.
- Replace path to GSL in `modules/gsl/core.scm` with a proper path to
  the library on your machine.
- Replace path to `libguilegslblas` in `modules/gsl/blas.scm` with a
  path to compiled `libguilegslblas.so`.
- Compile Guile files.
- Load them and use the library.
