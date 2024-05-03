(define-module (gsl eigensystems)
  #:use-module (gsl utils)
  #:use-module ((gsl matrices) #:prefix mtx:)
  #:use-module ((gsl vectors) #:prefix vec:)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:export (alloc
            free
            solve!))

(define alloc
  (foreign-fn "gsl_eigen_symmv_alloc" (list size_t) '*))

(define free
  (foreign-fn "gsl_eigen_symmv_free" '(*) void))

(define* (solve! mtx #:key
                 (evalues-vec (vec:alloc (mtx:columns mtx)))
                 (evectors-mtx (mtx:alloc (mtx:columns mtx) (mtx:columns mtx))))
  "Put MTX eigenvalues into EVALUES-VEC & eigenvectors in EVECTORS-MTX.
Return (EVALUES-VEC EVECTORS-MTX) list.
If EVALUES-VEC and/or EVECTORS-MTX are not provided, allocate and
return the new ones. Otherwise use the provided ones.
Modifies the MTX."
  (let ((workspace (alloc (mtx:columns mtx))))
    ((foreign-fn "gsl_eigen_symmv" '(* * * *) int)
     (mtx:unwrap mtx) (vec:unwrap evalues-vec) (mtx:unwrap evectors-mtx) workspace)
    (free workspace)
    (list evalues-vec evectors-mtx)))
