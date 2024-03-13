(define-module (gsl blas)
  #:use-module (gsl core)
  #:use-module (gsl vectors)
  #:use-module (gsl matrices)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:export (+row-major+
            +col-major+
            +column-major+
            +no-trans+
            +trans+
            +conj-trans+
            +no-transpose+
            +transpose+
            +conjugate-transpose+
            +upper+
            +lower+
            +non-unit+
            +unit+
            +left+
            +right+
            ;; Level 1
            ddot
            dnrm2
            dasum
            idamax
            dswap!
            dcopy!
            daxpy!
            dscal!
            drotg
            drot
            drotmg
            drotm
            ;; Level 2
            dgemv!
            dtrmv!
            dtrsv!
            dsymv!
            dger!
            dsyr!
            dsyr2!
            ;; Level 3
            dgemm!
            dsymm!
            dtrmm!
            dtrsm!
            dsyrk!
            dsyrk2!))

;; TODO: What are default-ish values for order, uplo, diag, and side?
;; Order.
(define +row-major+ 101)
(define +col-major+ 102)
(define +column-major+ +col-major+)
;; Transpose
(define +no-trans+ 111)
(define +trans+ 112)
(define +conj-trans+ 113)
(define +no-transpose+ +no-trans+)
(define +transpose+ +trans+)
(define +conjugate-transpose+ +conj-trans+)
;; Upper/lower
(define +upper+ 121)
(define +lower+ 122)
;; Diagonals
(define +non-unit+ 131)
(define +unit+ 132)
;; Side
(define +left+ 141)
(define +right+ 142)

;; FIXME: No floats & complex, only double-precision for now

(define* (blas-fn name arg-types #:optional (return-type int))
  (foreign-fn (string-append "gsl_blas_" name) arg-types return-type))

;; Level 1

(define (ddot vec1 vec2)
  (let ((result (make-c-ptr double)))
    ((blas-fn "ddot" '(* * *)) vec1 vec2 result)
    (first (parse-c-struct result (list double)))))

(define dnrm2
  (blas-fn "dnrm2" '(*) double))

(define dasum
  (blas-fn "dasum" '(*) double))

(define (idamax vec)
  (blas-fn "idamax" '(*) size_t))

(define dswap!
  (blas-fn "dswap" '(* *)))

(define dcopy!
  (blas-fn "dcopy" '(* *)))

(define daxpy!
  (blas-fn "daxpy" `(,double * *)))

(define dscal!
  (blas-fn "dscal" `(,double *) void))

;; TODO: Figure out what the result is and return it instead.
;; TODO: Figure out if they are destructive (they must be).
(define drotg
  (blas-fn "drotg" '(* * * *)))
(define drot
  (blas-fn "drot" `(* * ,double ,double)))
(define drotmg
  (blas-fn "drotmg" `(* * * ,double *)))
(define drotm
  (blas-fn "drotm" `(* * *)))

;; Level 2

(define* (dgemv! amtx xvec yvec #:key (alpha 1.0) (beta 1.0) (transpose +no-trans+))
  ((blas-fn "dgemv" `(,int ,double * * ,double *))
   transpose alpha amtx xvec beta yvec))

(define* (dtrmv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn "dtrmv" `(,int ,int ,int * *))
   uplo transpose diag amtx xvec))

(define* (dtrsv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn "dtrsv" `(,int ,int ,int * *))
   uplo transpose diag amtx xvec))

(define* (dsymv! mtx xvec yvec #:key (uplo +upper+) (alpha 1.0) (beta 1.0))
  ((blas-fn "dsymv" `(,int ,double * * ,double *))
   uplo alpha mtx xvec beta yvec))

(define* (dger! xvec yvec mtx #:key (alpha 1.0))
  ((blas-fn "dger" `(,double * * *))
   alpha xvec yvec mtx))

(define* (dsyr! xvec amtx #:key (uplo +upper+) (alpha 1.0))
  ((blas-fn "dsyr" `(,int ,double * *))
   uplo alpha xvec amtx))

(define* (dsyr2! xvec yvec amtx #:key (uplo +upper+) (alpha 1.0))
  ((blas-fn "dsyr2" `(,int ,double * * *))
   uplo alpha xvec yvec amtx))

;; Level 3

(define* (dgemm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  ((blas-fn "dgemm" `(,int ,int ,double * * ,double *))
   transpose-a transpose-b alpha amtx bmtx beta cmtx))

(define* (dsymm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (side +right+) (uplo +upper+))
  ((blas-fn "dsymm" `(,int ,int ,double * * ,double *))
   side uplo alpha amtx bmtx beta cmtx))

(define* (dtrmm! amtx bmtx #:key (alpha 1.0) (side +right+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn "dtrmm" `(,int ,int ,int ,int ,double * *))
   side uplo transpose diag alpha amtx bmtx))

(define* (dtrsm! amtx bmtx #:key (alpha 1.0) (side +right+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn "dtrsm" `(,int ,int ,int ,int ,double * *))
   side uplo transpose diag alpha amtx bmtx))

(define* (dsyrk! amtx cmtx #:key (alpha 1.0) (beta 1.0) (uplo +upper+) (transpose +no-trans+))
  ((blas-fn "dsyrk" `(,int ,int ,double * ,double *))
   uplo transpose alpha amtx beta cmtx))

(define* (dsyrk2! amtx cmtx #:key (alpha 1.0) (beta 1.0) (uplo +upper+) (transpose +no-trans+))
  ((blas-fn "dsyrk2" `(,int ,int ,double * ,double *))
   uplo transpose alpha amtx beta cmtx))
