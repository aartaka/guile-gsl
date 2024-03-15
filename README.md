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
(use-modules ((gsl matrices) #:prefix mtx:))
(use-modules ((gsl blas) #:prefix blas:))
(let ((a (mtx:alloc 2 3 #(#(0.11 0.12 0.13)
                          #(0.21 0.22 0.23))))
      (b (mtx:alloc 3 2 #(#(1011 1012)
                          #(1021 1022)
                          #(1031 1032))))
      (result (mtx:alloc 2 2 0)))
  (blas:dgemm! a b result #:beta 0)
  (mtx:->2d-vector result))
;;; $1 = #(#(367.76 368.12) #(674.0600000000001 674.72))
```

## Names & How to Use The Library

Most bindings follow the GSL names, except that
- `gsl_` prefix and area prefix are dropped.
  - Area prefixes are modules now:
    - `(gsl vectors)` for vector ops.
    - `(gsl matrices)` for matrices.
    - `(gsl blas)` for BLAS compatibility.
    - `(gsl stat)` for statistics.
- Destructive operations have an exclamation mark (`!`) after the
  name.
- Copying counterparts to the destructive operations (or otherwise
  "safe" operations) don't have an exclamation mark.
- Transformation/conversion operations start with `->`.
- Some operations are renamed to better fit Scheme conventions.

To exemplify:
- `gsl_vector_alloc` is mere `alloc` in `(gsl vectors)`.
- `gsl_matrix_set` is `set!` in `(gsl matrices)`.
- `gsl_matrix_set_all` is `fill!` in `(gsl matrices)`.
- Conversion from `gsl_vector` to Scheme vector is `(gsl vectors) ->vector`

Notice that this extremely succinct naming means that many symbols in
the programs one writes (or even in standard Scheme, like `set!`) are
doomed to collide with the names provided by this library. Thus, use
the provided modules with suitable prefixes instead of importing them
raw:

``` scheme
(use-modules ((gsl matrices) #:prefix mtx:))
(use-modules ((gsl vectors) #:prefix vec:))
(use-modules ((gsl blas) #:prefix blas:))
```

Overall, you're best served by interactive help facilities Guile
provides:
``` scheme
> ,describe mtx:alloc
Create a new ROWSxCOLUMNS gsl_matrix.
FILL might be one of:
- #f for uninitialized matrix (garbage values, use `calloc' for
  zero-initialized or numeric FILL for constant-initialized matrices).
- Real number to fill the matrix with the same double value.
- Or a list/vector or lists/vectors with numbers to fill in.
> ,apropos dgemm
(gsl blas): dgemm!	#<procedure dgemm! (amtx bmtx cmtx #:key alpha beta transpose-a transpose-b)>
```
