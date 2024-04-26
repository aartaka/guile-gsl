(define-module (gsl blas)
  #:use-module (gsl core)
  #:use-module ((gsl vectors) #:prefix vec:)
  #:use-module ((gsl matrices)  #:prefix mtx:)
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
            dot
            ddot
            nrm2
            dnrm2
            asum
            dasum
            iamax
            idamax
            swap!
            dswap!
            copy!
            dcopy!
            axpy!
            daxpy!
            scal!
            dscal!
            scale!
            drotg
            drot
            drotmg
            drotm
            ;; Level 2
            gemv!
            dgemv!
            trmv!
            dtrmv!
            trsv!
            dtrsv!
            symv!
            dsymv!
            ger!
            dger!
            syr!
            dsyr!
            syr2!
            dsyr2!
            ;; Level 3
            gemm!
            dgemm!
            symm!
            dsymm!
            trmm!
            dtrmm!
            trsm!
            dtrsm!
            syrk!
            dsyrk!
            syrk2!
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

;; FIXME: No complex, only float and double for now

(define* (blas-fn name arg-types #:optional (return-type int))
  (foreign-fn (string-append "gsl_blas_" name) arg-types return-type))

;; Level 1

(define (dispatch thing f64 f32)
  (cond
   ((and (mtx:mtx? thing)
         (eq? 'f32 (mtx:type thing)))
    f32)
   ((mtx:mtx? thing)
    f64)
   ((and (vec:vec? thing)
         (eq? 'f32 (vec:type thing)))
    f32)
   ((vec:vec? thing)
    f64)
   ((pointer? thing)
    f64)))

(define (dot vec1 vec2)
  (dispatch vec1
            (let ((result (make-c-ptr double)))
              ((blas-fn "ddot" '(* * *)) (vec:unwrap vec1) (vec:unwrap vec2) result)
              (first (parse-c-struct result (list double))))
            (let ((result (make-c-ptr float)))
              ((blas-fn "sdot" '(* * *)) (vec:unwrap vec1) (vec:unwrap vec2) result)
              (first (parse-c-struct result (list float))))))
(define ddot dot)
(define (nrm2 vec)
  ((dispatch vec
             (blas-fn "dnrm2" '(*) double)
             (blas-fn "snrm2" '(*) float))
   (vec:unwrap vec)))
(define dnrm2 nrm2)

(define (asum vec)
  ((dispatch vec
             (blas-fn "dasum" '(*) double)
             (blas-fn "sasum" '(*) float))
   (vec:unwrap vec)))
(define dasum asum)

(define (iamax vec)
  ((blas-fn (dispatch vec "idamax" "isamax") '(*) size_t)
   (vec:unwrap vec)))
(define idamax iamax)

(define (swap! vec1 vec2)
  ((blas-fn (dispatch vec1
                      "dswap"
                      "sswap") '(* *))
   (vec:unwrap vec1) (vec:unwrap vec2)))
(define dswap! swap!)

(define (copy! vec1 vec2)
  ((blas-fn (dispatch vec1
                      "dcopy"
                      "scopy") '(* *))
   (vec:unwrap vec1) (vec:unwrap vec2)))
(define dcopy! copy!)

(define (axpy! alpha x y)
  ((dispatch x
             (blas-fn "daxpy" `(,double * *))
             (blas-fn "saxpy" `(,float * *)))
   alpha (vec:unwrap x) (vec:unwrap y)))
(define daxpy! axpy!)

(define (scal! alpha vec)
  ((dispatch vec
             (blas-fn "dscal" `(,double *) void)
             (blas-fn "sscal" `(,float *) void))
   alpha (vec:unwrap vec)))
(define scale! scal!)
(define dscal! scal!)

;; TODO: Figure out what the result is and return it instead.
;; TODO: Wrap them for f32/f64 distinction
(define drotg
  (blas-fn "drotg" '(* * * *)))
(define drot
  (blas-fn "drot" `(* * ,double ,double)))
(define drotmg
  (blas-fn "drotmg" `(* * * ,double *)))
(define drotm
  (blas-fn "drotm" `(* * *)))

;; Level 2

(define* (gemv! amtx xvec yvec #:key (alpha 1.0) (beta 1.0) (transpose +no-trans+))
  ((dispatch amtx
             (blas-fn "dgemv" `(,int ,double * * ,double *))
             (blas-fn "sgemv" `(,int ,float * * ,float *)))
   transpose alpha (mtx:unwrap amtx) (vec:unwrap xvec) beta (vec:unwrap yvec)))
(define dgemv! gemv!)

(define* (trmv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn (dispatch amtx
                      "dtrmv"
                      "strmv") `(,int ,int ,int * *))
   uplo transpose diag (mtx:unwrap amtx) (vec:unwrap xvec)))
(define dtrmv! trmv!)

(define* (trsv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((blas-fn (dispatch amtx
                      "dtrsv"
                      "strsv") `(,int ,int ,int * *))
   uplo transpose diag (mtx:unwrap amtx) (vec:unwrap xvec)))
(define dtrsv! trsv!)

(define* (symv! amtx xvec yvec #:key (uplo +upper+) (alpha 1.0) (beta 1.0))
  ((dispatch amtx
             (blas-fn "dsymv" `(,int ,double * * ,double *))
             (blas-fn "ssymv" `(,int ,float * * ,float *)))
   uplo alpha (mtx:unwrap amtx) (vec:unwrap xvec) beta (vec:unwrap yvec)))
(define dsymv! symv!)

(define* (ger! xvec yvec amtx #:key (alpha 1.0))
  ((dispatch xvec
             (blas-fn "dger" `(,double * * *))
             (blas-fn "sger" `(,float * * *)))
   alpha (vec:unwrap xvec) (vec:unwrap yvec) (mtx:unwrap amtx)))
(define dger! ger!)

(define* (syr! xvec amtx #:key (uplo +upper+) (alpha 1.0))
  ((dispatch xvec
             (blas-fn "dsyr" `(,int ,double * *))
             (blas-fn "ssyr" `(,int ,float * *)))
   uplo alpha (vec:unwrap xvec) (mtx:unwrap amtx)))
(define dsyr! syr!)

(define* (syr2! xvec yvec amtx #:key (uplo +upper+) (alpha 1.0))
  ((dispatch xvec
             (blas-fn "dsyr2" `(,int ,double * * *))
             (blas-fn "ssyr2" `(,int ,float * * *)))
   uplo alpha (vec:unwrap xvec) (vec:unwrap yvec) (mtx:unwrap amtx)))
(define dsyr2! syr2!)

;; Level 3

(define* (gemm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  ((dispatch amtx
             (blas-fn "dgemm" `(,int ,int ,double * * ,double *))
             (blas-fn "sgemm" `(,int ,int ,float * * ,float *)))
   transpose-a transpose-b alpha (mtx:unwrap amtx) (mtx:unwrap bmtx) beta (mtx:unwrap cmtx)))
(define dgemm! gemm!)

(define* (symm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (side +right+) (uplo +upper+))
  ((dispatch amtx
             (blas-fn "dsymm" `(,int ,int ,double * * ,double *))
             (blas-fn "dsymm" `(,int ,int ,float * * ,float *)))
   side uplo alpha (mtx:unwrap amtx) (mtx:unwrap bmtx) beta (mtx:unwrap cmtx)))
(define dsymm! symm!)

(define* (trmm! amtx bmtx #:key (alpha 1.0) (side +right+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((dispatch amtx
             (blas-fn "dtrmm" `(,int ,int ,int ,int ,double * *))
             (blas-fn "strmm" `(,int ,int ,int ,int ,float * *)))
   side uplo transpose diag alpha (mtx:unwrap amtx) (mtx:unwrap bmtx)))
(define dtrmm! trmm!)

(define* (trsm! amtx bmtx #:key (alpha 1.0) (side +right+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  ((dispatch amtx
             (blas-fn "dtrsm" `(,int ,int ,int ,int ,double * *))
             (blas-fn "strsm" `(,int ,int ,int ,int ,float * *)))
   side uplo transpose diag alpha (mtx:unwrap amtx) (mtx:unwrap bmtx)))
(define dtrsm! trsm!)

(define* (syrk! amtx cmtx #:key (alpha 1.0) (beta 1.0) (uplo +upper+) (transpose +no-trans+))
  ((dispatch amtx
             (blas-fn "dsyrk" `(,int ,int ,double * ,double *))
             (blas-fn "ssyrk" `(,int ,int ,float * ,float *)))
   uplo transpose alpha (mtx:unwrap amtx) beta (mtx:unwrap cmtx)))
(define dsyrk! syrk!)

(define* (syrk2! amtx cmtx #:key (alpha 1.0) (beta 1.0) (uplo +upper+) (transpose +no-trans+))
  ((dispatch amtx
             (blas-fn "dsyrk2" `(,int ,int ,double * ,double *))
             (blas-fn "ssyrk2" `(,int ,int ,float * ,float *)))
   uplo transpose alpha (mtx:unwrap amtx) beta (mtx:unwrap cmtx)))
(define dsyrk2! syrk2!)
