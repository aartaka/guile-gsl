(define-module (gsl linear-algebra)
  #:use-module (gsl utils)
  #:use-module (gsl core)
  #:use-module ((gsl matrices) #:prefix mtx:)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:export (decompose
            %invert
            %invert!
            invert
            %determinant
            %determinant-log
            %determinant-sign
            determinant))

(define (decompose mtx)
  (assert-types mtx mtx:mtx?)
  (let ((signum (make-c-ptr int 0))
        (permutation ((foreign-fn "gsl_permutation_alloc" (list size_t) '*)
                      (mtx:rows mtx))))
    ((foreign-fn "gsl_linalg_LU_decomp" '(* * *))
     (mtx:unwrap mtx) permutation signum)
    (values mtx permutation (first (parse-c-struct signum (list int))))))

(define* (%invert decomposed permutation
                  #:optional (inverse (mtx:alloc (mtx:columns decomposed) (mtx:rows decomposed))))
  ((foreign-fn "gsl_linalg_LU_invert" '(* * *) int)
   (mtx:unwrap decomposed) permutation (mtx:unwrap inverse))
  inverse)
(define (%invert! decomposed permutation)
  ((foreign-fn "gsl_linalg_LU_invx" '(* *) int)
   (mtx:unwrap decomposed) permutation)
  decomposed)

(define* (invert mtx)
  (let ((copy (mtx:copy! mtx)))
    (receive (decomposed permutations signum)
        (decompose copy)
      (%invert! decomposed permutations)
      decomposed)))

(define (%determinant decomposed signum)
  ((foreign-fn "gsl_linalg_LU_det" (list '* int) double)
   (mtx:unwrap decomposed) signum))
(define (%determinant-log decomposed)
  ((foreign-fn "gsl_linalg_LU_lndet" (list '*) double)
   (mtx:unwrap decomposed)))
(define (%determinant-sign decomposed signum)
  ((foreign-fn "gsl_linalg_LU_sgndet" (list '* int) int)
   (mtx:unwrap decomposed) signum))
(define (determinant mtx)
  (let* ((copy (mtx:copy! mtx)))
    (receive (decomposed permutations signum)
        (decompose copy)
      (let ((det (%determinant decomposed signum))
            (det-log (%determinant-log decomposed))
            (det-sign (%determinant-sign decomposed signum)))
        (mtx:free copy)
        (values det det-log det-sign)))))
