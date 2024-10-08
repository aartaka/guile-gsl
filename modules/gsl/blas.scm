(define-module (gsl blas)
  #:use-module (gsl utils)
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
            gemv
            trmv!
            dtrmv!
            trsv!
            dtrsv!
            symv!
            dsymv!
            symv
            ger!
            dger!
            ger
            syr!
            dsyr!
            syr
            syr2!
            dsyr2!
            syr2
            ;; Level 3
            gemm!
            dgemm!
            gemm
            symm!
            dsymm!
            symm
            trmm!
            dtrmm!
            trsm!
            dtrsm!
            syrk!
            dsyrk!
            syrk
            syr2k!
            dsyr2k!
            syr2k
            ;; Generic ops
            gem!
            gem
            sym!
            sym
            trm!
            trs!))

(define (check-types . objects)
  (let ((types (map (lambda (obj)
                      (if (mtx:mtx? obj)
                          (mtx:type obj)
                          (vec:type obj)))
                    objects)))
    (unless (reduce eq? (car types) (cdr types))
      (error (format #t "Cannot operate on objects of different type: ~s (typed as ~s)" objects types)))))

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

(define (ensure-transpose transpose?)
  (assert-types transpose? boolean? integer?)
  (if (integer? transpose?)
      transpose?
      (if transpose?
          +transpose+
          +no-transpose+)))

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

(define (dot xvec yvec)
  (check-types xvec yvec)
  (dispatch xvec
            (let ((result (make-c-ptr double)))
              ((blas-fn "ddot" '(* * *)) (vec:unwrap xvec) (vec:unwrap yvec) result)
              (first (parse-c-struct result (list double))))
            (let ((result (make-c-ptr float)))
              ((blas-fn "sdot" '(* * *)) (vec:unwrap xvec) (vec:unwrap yvec) result)
              (first (parse-c-struct result (list float))))))
(define ddot dot)
(define (nrm2 xvec)
  ((dispatch xvec
             (blas-fn "dnrm2" '(*) double)
             (blas-fn "snrm2" '(*) float))
   (vec:unwrap xvec)))
(define dnrm2 nrm2)

(define (asum xvec)
  ((dispatch xvec
             (blas-fn "dasum" '(*) double)
             (blas-fn "sasum" '(*) float))
   (vec:unwrap xvec)))
(define dasum asum)

(define (iamax xvec)
  ((blas-fn (dispatch xvec "idamax" "isamax") '(*) size_t)
   (vec:unwrap xvec)))
(define idamax iamax)

(define (swap! xvec yvec)
  (check-types xvec yvec)
  ((blas-fn (dispatch xvec
                      "dswap"
                      "sswap") '(* *))
   (vec:unwrap xvec) (vec:unwrap yvec)))
(define dswap! swap!)

(define (copy! xvec yvec)
  (check-types xvec yvec)
  ((blas-fn (dispatch xvec
                      "dcopy"
                      "scopy") '(* *))
   (vec:unwrap xvec) (vec:unwrap yvec)))
(define dcopy! copy!)

(define (axpy! alpha xvec yvec)
  (check-types xvec yvec)
  ((dispatch xvec
             (blas-fn "daxpy" `(,double * *))
             (blas-fn "saxpy" `(,float * *)))
   alpha (vec:unwrap xvec) (vec:unwrap yvec)))
(define daxpy! axpy!)

(define (scal! alpha xvec)
  ((dispatch xvec
             (blas-fn "dscal" `(,double *) void)
             (blas-fn "sscal" `(,float *) void))
   alpha (vec:unwrap xvec)))
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
  (check-types amtx xvec yvec)
  ((dispatch amtx
             (blas-fn "dgemv" `(,int ,double * * ,double *))
             (blas-fn "sgemv" `(,int ,float * * ,float *)))
   (ensure-transpose transpose) alpha (mtx:unwrap amtx) (vec:unwrap xvec) beta (vec:unwrap yvec)))
(define dgemv! gemv!)
(define* (gemv amtx xvec #:key (alpha 1.0) (transpose +no-trans+))
  "Non-destructive `gemv!', creating and returning YVEC."
  (check-types amtx xvec)
  (let ((yvec (vec:alloc (if (= (ensure-transpose transpose) +no-trans+)
                             (mtx:rows amtx)
                             (mtx:columns amtx))
                         0 (mtx:type amtx))))
    (gemv! amtx xvec yvec
           #:beta 0 #:alpha alpha #:transpose (ensure-transpose transpose))
    yvec))

(define* (trmv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  (check-types amtx xvec)
  ((blas-fn (dispatch amtx
                      "dtrmv"
                      "strmv") `(,int ,int ,int * *))
   uplo (ensure-transpose transpose) diag (mtx:unwrap amtx) (vec:unwrap xvec)))
(define dtrmv! trmv!)

(define* (trsv! amtx xvec #:key (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  (check-types amtx xvec)
  ((blas-fn (dispatch amtx
                      "dtrsv"
                      "strsv") `(,int ,int ,int * *))
   uplo (ensure-transpose transpose) diag (mtx:unwrap amtx) (vec:unwrap xvec)))
(define dtrsv! trsv!)

(define* (symv! amtx xvec yvec #:key (uplo +upper+) (alpha 1.0) (beta 1.0))
  (check-types amtx xvec yvec)
  ((dispatch amtx
             (blas-fn "dsymv" `(,int ,double * * ,double *))
             (blas-fn "ssymv" `(,int ,float * * ,float *)))
   uplo alpha (mtx:unwrap amtx) (vec:unwrap xvec) beta (vec:unwrap yvec)))
(define dsymv! symv!)
(define* (symv amtx xvec #:key (uplo +upper+) (alpha 1.0))
  "Non-destructive `symv!', creating and returning YVEC."
  (check-types amtx xvec)
  (let ((yvec (vec:alloc (mtx:rows amtx) 0 (mtx:type amtx))))
    (symv! amtx xvec yvec #:beta 0 #:alpha alpha #:uplo uplo)
    yvec))

(define* (ger! xvec yvec amtx #:key (alpha 1.0))
  (check-types xvec yvec amtx)
  ((dispatch xvec
             (blas-fn "dger" `(,double * * *))
             (blas-fn "sger" `(,float * * *)))
   alpha (vec:unwrap xvec) (vec:unwrap yvec) (mtx:unwrap amtx)))
(define dger! ger!)
(define* (ger xvec yvec #:key (alpha 1.0))
  "Non-destructive `ger!', creating and returning AMTX."
  (check-types xvec yvec)
  (let ((amtx (mtx:alloc (vec:length xvec) (vec:length yvec)
                         0 (vec:type xvec))))
    (ger! xvec yvec amtx #:alpha alpha)
    amtx))

(define* (syr! xvec amtx #:key (uplo +upper+) (alpha 1.0))
  (check-types xvec amtx)
  ((dispatch xvec
             (blas-fn "dsyr" `(,int ,double * *))
             (blas-fn "ssyr" `(,int ,float * *)))
   uplo alpha (vec:unwrap xvec) (mtx:unwrap amtx)))
(define dsyr! syr!)
(define* (syr xvec #:key (alpha 1.0))
  "Non-destructive `syr!', creating and returning AMTX."
  (let ((amtx (mtx:alloc (vec:length xvec) (vec:length xvec)
                         0 (vec:type xvec))))
    (syr! xvec amtx #:alpha alpha)
    amtx))

(define* (syr2! xvec yvec amtx #:key (uplo +upper+) (alpha 1.0))
  (check-types xvec yvec amtx)
  ((dispatch xvec
             (blas-fn "dsyr2" `(,int ,double * * *))
             (blas-fn "ssyr2" `(,int ,float * * *)))
   uplo alpha (vec:unwrap xvec) (vec:unwrap yvec) (mtx:unwrap amtx)))
(define dsyr2! syr2!)
(define* (syr2 xvec yvec #:key (alpha 1.0))
  "Non-destructive `syr2!' creating and returning AMTX."
  (check-types xvec yvec)
  (let ((amtx (mtx:alloc (vec:length xvec) (vec:length xvec)
                         0 (vec:type xvec))))
    (syr2! xvec yvec amtx #:alpha alpha)
    amtx))

;; Level 3

(define* (gemm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  (check-types amtx bmtx cmtx)
  ((dispatch amtx
             (blas-fn "dgemm" `(,int ,int ,double * * ,double *))
             (blas-fn "sgemm" `(,int ,int ,float * * ,float *)))
   (ensure-transpose transpose-a) (ensure-transpose transpose-b)
   alpha (mtx:unwrap amtx) (mtx:unwrap bmtx) beta (mtx:unwrap cmtx)))
(define dgemm! gemm!)
(define* (gemm amtx bmtx #:key (alpha 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  "Non-destructive version of `gemm!', creating and returning CMTX."
  (check-types amtx bmtx)
  (let ((cmtx (mtx:alloc (if (= +no-transpose+ (ensure-transpose transpose-a))
                             (mtx:rows amtx)
                             (mtx:columns amtx))
                         (if (= +no-transpose+ (ensure-transpose transpose-b))
                             (mtx:columns bmtx)
                             (mtx:rows bmtx))
                         0 (mtx:type amtx))))
    (gemm! amtx bmtx cmtx
           #:beta 0 #:alpha alpha
           #:transpose-a (ensure-transpose transpose-a)
           #:transpose-b (ensure-transpose transpose-b))
    cmtx))

(define* (symm! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (side +left+) (uplo +upper+))
  (check-types amtx bmtx cmtx)
  ((dispatch amtx
             (blas-fn "dsymm" `(,int ,int ,double * * ,double *))
             (blas-fn "dsymm" `(,int ,int ,float * * ,float *)))
   side uplo alpha (mtx:unwrap amtx) (mtx:unwrap bmtx) beta (mtx:unwrap cmtx)))
(define dsymm! symm!)
(define* (symm amtx bmtx #:key (alpha 1.0) (side +left+) (uplo +upper+))
  "Non-destructive version of `symm!', creating and returning CMTX."
  (check-types amtx bmtx)
  (let ((cmtx (if (= side +right+)
                  (mtx:alloc (mtx:rows bmtx) (mtx:columns amtx) 0 (mtx:type amtx))
                  (mtx:alloc (mtx:rows amtx) (mtx:columns bmtx) 0 (mtx:type amtx)))))
    (symm! amtx bmtx cmtx
           #:beta 0 #:alpha alpha #:side side #:uplo uplo)
    cmtx))

(define* (trmm! amtx bmtx #:key (alpha 1.0) (side +left+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  (check-types amtx bmtx)
  ((dispatch amtx
             (blas-fn "dtrmm" `(,int ,int ,int ,int ,double * *))
             (blas-fn "strmm" `(,int ,int ,int ,int ,float * *)))
   side uplo (ensure-transpose transpose) diag
   alpha (mtx:unwrap amtx) (mtx:unwrap bmtx)))
(define dtrmm! trmm!)

(define* (trsm! amtx bmtx #:key (alpha 1.0) (side +left+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  (check-types amtx bmtx)
  ((dispatch amtx
             (blas-fn "dtrsm" `(,int ,int ,int ,int ,double * *))
             (blas-fn "strsm" `(,int ,int ,int ,int ,float * *)))
   side uplo (ensure-transpose transpose) diag
   alpha (mtx:unwrap amtx) (mtx:unwrap bmtx)))
(define dtrsm! trsm!)

(define* (syrk! amtx cmtx #:key (alpha 1.0) (beta 1.0) (uplo +upper+) (transpose +no-trans+))
  (check-types amtx cmtx)
  ((dispatch amtx
             (blas-fn "dsyrk" `(,int ,int ,double * ,double *))
             (blas-fn "ssyrk" `(,int ,int ,float * ,float *)))
   uplo (ensure-transpose transpose)
   alpha (mtx:unwrap amtx) beta (mtx:unwrap cmtx)))
(define dsyrk! syrk!)
(define* (syrk amtx #:key (alpha 1.0) (uplo +upper+) (transpose +no-trans+))
  "Non-destructive `syrk!' creating and returning a CMTX."
  (let ((cmtx (mtx:alloc (mtx:rows amtx) (mtx:rows amtx) 0 (mtx:type amtx))))
    (syrk! amtx cmtx #:beta 0 #:alpha alpha
           #:uplo uplo #:transpose (ensure-transpose transpose))
    cmtx))

(define* (syr2k! amtx bmtx cmtx #:key (alpha 1.0) (beta 1.0) (transpose +no-trans+) (uplo +upper+))
  (check-types amtx bmtx cmtx)
  ((dispatch amtx
             (blas-fn "dsyr2k" `(,int ,int ,double * * ,double *))
             (blas-fn "ssyr2k" `(,int ,int ,float * * ,float *)))
   uplo (ensure-transpose transpose)
   alpha (mtx:unwrap amtx) (mtx:unwrap bmtx) beta (mtx:unwrap cmtx)))
(define dsyr2k! syr2k!)
(define* (syr2k amtx bmtx #:key (alpha 1.0) (transpose +no-trans+) (uplo +upper+))
  "Non-destructive version of `syr2k!', creating and returning CMTX."
  (check-types amtx bmtx)
  (let ((cmtx (if (= (ensure-transpose transpose) +no-trans+)
                  (mtx:alloc (mtx:rows amtx) (mtx:rows amtx)
                             0 (mtx:type amtx))
                  (mtx:alloc (mtx:columns amtx) (mtx:columns amtx)
                             0 (mtx:type amtx)))))
    (syr2k! amtx bmtx cmtx
            #:beta 0 #:alpha alpha
            #:transpose (ensure-transpose transpose) #:uplo uplo)
    cmtx))


;; Generic operations: both mtx-vec and mtx-mtx

(define* (gem! amtx bsmth csmth #:key (alpha 1.0) (beta 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  "Multiply AMTX by BSMTH using `gemm!' if BSMTH is a matrix, `gemv!' otherwise."
  (check-types amtx bsmth csmth)
  (if (mtx:mtx? bsmth)
      (gemm! amtx bsmth csmth
             #:alpha alpha #:beta beta
             #:transpose-a (ensure-transpose transpose-a)
             #:transpose-b (ensure-transpose transpose-b))
      (gemv! amtx bsmth csmth
             #:alpha alpha #:beta beta
             #:transpose (ensure-transpose transpose-a))))
(define* (gem amtx bsmth #:key (alpha 1.0) (transpose-a +no-trans+) (transpose-b +no-trans+))
  "Non-destructive version of `gem!'.
Returns CSMTH, matrix of vector."
  (check-types amtx bsmth)
  (if (mtx:mtx? bsmth)
      (gemm amtx bsmth
            #:alpha alpha
            #:transpose-a (ensure-transpose transpose-a)
            #:transpose-b (ensure-transpose transpose-b))
      (gemv amtx bsmth
            #:alpha alpha
            #:transpose (ensure-transpose transpose-a))))

(define* (sym! amtx bsmth csmth #:key (alpha 1.0) (beta 1.0) (side +left+) (uplo +upper+))
  "Symmetric AMTX multiply by BSMTH.
If BSMTH is a matrix, use `symm!', otherwise use `symv!'."
  (check-types amtx bsmth csmth)
  (if (mtx:mtx? bsmth)
      (symm! amtx bsmth csmth
             #:alpha alpha #:beta beta
             #:side side #:uplo uplo)
      (symv! amtx bsmth csmth
             #:alpha alpha #:beta beta #:uplo uplo)))
(define* (sym amtx bsmth #:key (alpha 1.0) (side +left+) (uplo +upper+))
  "Non-destructive version of `sym!'.
Returns CSMTH as a result of multiplication."
  (check-types amtx bsmth)
  (if (mtx:mtx? bsmth)
      (symm amtx bsmth
            #:alpha alpha
            #:side side #:uplo uplo)
      (symv amtx bsmth
            #:alpha alpha #:uplo uplo)))

(define* (trm! amtx bsmth #:key (alpha 1.0) (side +left+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  "Triangular AMTX multiplication by BSMTH.
If BSMTH is a matrix, use `trmm!', otherwise `trmv!'."
  (check-types amtx bsmth)
  (if (mtx:mtx? bsmth)
      (trmm! amtx bsmth
             #:alpha alpha #:side side
             #:uplo uplo #:transpose (ensure-transpose transpose) #:diag diag)
      (trmv! amtx bsmth
             #:uplo uplo #:transpose (ensure-transpose transpose) #:diag diag)))

(define* (trs! amtx bsmth #:key (alpha 1.0) (side +left+) (uplo +upper+) (transpose +no-trans+) (diag +non-unit+))
  "Multiplication of inverse of AMTX by BSMTH.
Use `trsm!' if BSMTH is matrix, `trsv!' otherwise."
  (check-types amtx bsmth)
  (if (mtx:mtx? bsmth)
      (trsm! amtx bsmth
             #:alpha alpha #:side side
             #:uplo uplo #:transpose (ensure-transpose transpose) #:diag diag)
      (trsv! amtx bsmth
             #:uplo uplo #:transpose (ensure-transpose transpose) #:diag diag)))
