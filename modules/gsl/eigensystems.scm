(define-module (gsl eigensystems)
  #:use-module (gsl utils)
  #:use-module ((gsl matrices) #:prefix mtx:)
  #:use-module ((gsl vectors) #:prefix vec:)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:export (alloc
            free
            solve!
            sort!))

(define alloc
  (foreign-fn "gsl_eigen_symmv_alloc" (list size_t) '*))

(define free
  (foreign-fn "gsl_eigen_symmv_free" '(*) void))

(define* (solve! mtx #:key
                 (evalues-vec (vec:alloc (mtx:columns mtx)))
                 (evectors-mtx (mtx:alloc (mtx:columns mtx) (mtx:columns mtx))))
  "Put MTX eigenvalues into EVALUES-VEC & eigenvectors in EVECTORS-MTX.
Return (EVALUES-VEC EVECTORS-MTX RESIDUALS-MTX) list.
If EVALUES-VEC and/or EVECTORS-MTX are not provided, allocate and
return the new ones. Otherwise use the provided ones.
Modifies the MTX and returns it as RESIDUALS-MTX."
  (let ((workspace (alloc (mtx:columns mtx))))
    ((foreign-fn "gsl_eigen_symmv" '(* * * *) int)
     (mtx:unwrap mtx) (vec:unwrap evalues-vec) (mtx:unwrap evectors-mtx) workspace)
    (free workspace)
    (values evalues-vec evectors-mtx mtx)))
(define* (solve mtx #:key
                 (evalues-vec (vec:alloc (mtx:columns mtx)))
                 (evectors-mtx (mtx:alloc (mtx:columns mtx) (mtx:columns mtx))))
  "Same as `solve!' but creates a new matrix for computations.
MTX remains unchanged."
  (let* ((copy (mtx:copy! mtx)))
    (receive (evalues-vec evectors-mtx residuals-mtx)
        (solve! copy
                #:evalues-vec evalues-vec #:evectors-mtx evectors-mtx)
      (mtx:free copy)
      ;; A more elegant way to do this? Something like CL's
      ;; values-list?
      (values evalues-vec evectors-mtx residuals-mtx))))

(define* (sort! evalues-vec evectors-mtx #:optional (ascending? #t))
  ((foreign-fn "gsl_eigen_symmv_sort" `(* * ,int))
   (vec:unwrap evalues-vec) (mtx:unwrap evectors-mtx)
   (if ascending?
       ;; GSL_EIGEN_SORT_VAL_ASC
       0
       ;; GSL_EIGEN_SORT_VAL_DESC
       1)))
