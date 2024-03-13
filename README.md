# Guile GSL Bindings

This binds a minimal set of GSL functions (the ones needed for the
guile-gemma project, at least—contribute more if you feel like it) for
use in Guile.

## Getting Started

Install this library to where Guile can find it (Guix package
definition is already provided) and start the REPL. 
`use-modules` the ones you need and make your computations. 
Here's a BLAS example from the GSL docs, adjusted to Scheme:

``` scheme
(use-modules (gsl matrices))
(use-modules (gsl blas))
(let ((a (mtx-alloc 2 3 #(#(0.11 0.12 0.13)
                          #(0.21 0.22 0.23))))
      (b (mtx-alloc 3 2 #(#(1011 1012)
                          #(1021 1022)
                          #(1031 1032))))
      (result (mtx-alloc 2 2 0)))
  (dgemm! a b result #:beta 0)
  (mtx->2d-vector result))
;;; $1 = #(#(367.76 368.12) #(674.0600000000001 674.72))
```

## Names & How to Use The Library

Most bindings follow the GSL names, except for `gsl_` prefix being dropped and area prefixes shortened:
- Vector functions start with `vec-`
- Matrix functions start with `mtx-`
- BLAS functions are not prefixed with anything due to their *unique*
  naming conventions.

Notice that area prefixes might be dropped at some moment. But, even
so, you can use the old names by `use-modules`-ing with custom prefix:

``` scheme
(use-modules ((gsl matrices) #:prefix mtx-))
(use-modules ((gsl vectors) #:prefix vec-))
```

Destructive operations (all of BLAS and some operations for vectors
and matrices) are suffixed with `!`, as customary in Scheme.

Overall, you're best served by interactive help facilities Guile
provides:
``` scheme
> ,describe mtx-alloc
Create a new ROWSxCOLUMNS gsl_matrix.
FILL might be one of:
- #f for uninitialized matrix (garbage values, use `mtx-calloc' for
  zero-initialized or numeric FILL for constant-initialized matrices).
- Real number to fill the matrix with the same double value.
> ,apropos dgemm
(gsl blas): dgemm!	#<procedure dgemm! (amtx bmtx cmtx #:key alpha beta transpose-a transpose-b)>
```
