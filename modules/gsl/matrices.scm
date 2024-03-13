(define-module (gsl matrices)
  #:use-module (gsl core)
  #:use-module (gsl vectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (;; Access
            mtx-parts
            mtx-data
            mtx-dimensions
            mtx-rows
            mtx-columns
            mtx-get
            mtx-set!
            mtx-ptr
            ;; (De)allocation
            mtx-alloc
            mtx-calloc
            mtx-free
            mtx-copy!
            mtx-copy
            mtx->2d-vector
            ;; Predicates
            mtx-null?
            mtx-positive?
            mtx-negative?
            mtx-non-negative?
            mtx-equal?
            ;; Aggregates
            mtx-min
            mtx-min-index
            mtx-max
            mtx-max-index
            ;; Views
            mtx-row
            mtx-column
            mtx-diagonal
            mtx-subdiagonal
            mtx-superdiagonal
            mtx-subrow
            mtx-subcolumn
            ;; Matrix<->vector
            mtx-row->vec!
            mtx-column->vec!
            vec->mtx-row!
            vec->mtx-column!
            ;; Row/column ops.
            mtx-transpose!
            ;; Ops
            mtx-add!
            mtx-add
            mtx-subtract!
            mtx-subtract
            mtx-multiply-elements!
            mtx-multiply-elements
            mtx-divide-elements!
            mtx-divide-elements
            mtx-scale!
            mtx-scale
            mtx-scale-columns!
            mtx-scale-columns
            mtx-scale-rows!
            mtx-scale-rows
            mtx-add-constant!
            mtx-add-constant
            ;; Helpers
            call-with-mtx
            for-mtx
            ensure-gsl))

;; Access
(define (mtx-parts mtx)
  "Return parts of MTX struct:
- rows (int)
- columns (int)
- tda??? (int)
- data (pointer)
- block (pointer)
- owner (int)"
  (parse-c-struct mtx (list size_t size_t size_t '*   '*    int)))
(define (mtx-data mtx)
  "Return raw MTX data array (of doubles) pointer."
  (fourth (mtx-parts mtx)))
(define (mtx-dimensions mtx)
  "Return a list of (ROWS COLUMNS) in MTX."
  (take (mtx-parts mtx) 2))
(define (mtx-rows mtx)
  (first (mtx-dimensions mtx)))
(define (mtx-columns mtx)
  (second (mtx-dimensions mtx)))

(define (mtx-get mtx row column)
  ((foreign-fn "gsl_matrix_get" `(* ,size_t ,size_t) double)
   mtx row column))
(define (mtx-set! mtx row column val)
  ((foreign-fn "gsl_matrix_set" `(* ,size_t ,size_t ,double) void)
   mtx row column val))
(define (mtx-ptr mtx row column)
  "Return a pointer to [ROW][COLUMN]-th element of MTX."
  ((foreign-fn "gsl_matrix_ptr" `(* ,size_t ,size_t) '*)
   mtx row column))

;; (De)allocation
(define* (mtx-alloc rows columns #:optional (fill #f))
  "Create a new ROWSxCOLUMNS gsl_matrix.
FILL might be one of:
- #f for uninitialized matrix (garbage values, use `mtx-calloc' for
  zero-initialized or numeric FILL for constant-initialized matrices).
- Real number to fill the matrix with the same double value.
- Or a list/vector or lists/vectors with numbers to fill in."
  (let ((mtx ((foreign-fn "gsl_matrix_alloc" (list size_t size_t) '*)
              rows columns)))
    (when fill
      (cond
       ((number? fill)
        ((foreign-fn "gsl_matrix_set_all" `(* ,double) void)
         mtx fill))
       ((sequence? fill)
        (for-sequence
         (lambda (row vec)
           (for-sequence
            (lambda (column elem)
              (when (and (< row (mtx-rows mtx))
                         (< column (mtx-columns mtx)))
                (mtx-set! mtx row column elem)))
            vec))
         fill))
       (else
        (error "Don't know how to fill a matrix with" fill))))
    mtx))
(define mtx-calloc
  "Allocate a new matrix of ROWxCOLUMNS initialized to 0."
  (foreign-fn "gsl_matrix_calloc" (list size_t size_t) '*))
(define mtx-free
  (foreign-fn "gsl_matrix_free" '(*) void))

(define (mtx-fill! mtx fill)
  ((foreign-fn "gsl_matrix_set_all" `(* ,double) void)
   mtx fill))

(define (mtx-copy! src dest)
  ;; FIXME: This implementation kills the process.
  ;; ((foreign-fn "gsl_matrix_memcpy" '(* *) int)
  ;;  dest src)
  (let row-rec ((row 0))
    (when (< row (mtx-rows src))
      (let column-rec ((column 0))
        (when (< column (mtx-columns src))
          (mtx-set! dest row column
                    (mtx-get src row column))
          (column-rec (1+ column))))
      (row-rec (1+ row)))))
(define (mtx-copy src)
  "Non-destructive version of `mtx-copy~', returns a new matrix like SRC."
  (let ((new-mtx (mtx-alloc (mtx-rows src) (mtx-columns src))))
    (mtx-copy! src new-mtx)
    new-mtx))

(define (mtx->2d-vector mtx)
  "Convert MTX to a Scheme vector of vectors of numbers."
  (vector-unfold
   (lambda (row)
     (vector-unfold
      (lambda (column)
        (mtx-get mtx row column))
      (mtx-columns mtx)))
   (mtx-rows mtx)))

;; Predicates
(define (mtx-null? mtx)
  (positive? ((foreign-fn "gsl_matrix_isnull" '(*) int) mtx)))
(define (mtx-positive? mtx)
  (positive? ((foreign-fn "gsl_matrix_ispos" '(*) int) mtx)))
(define (mtx-negative? mtx)
  (positive? ((foreign-fn "gsl_matrix_isneg" '(*) int) mtx)))
(define (mtx-non-negative? mtx)
  (positive? ((foreign-fn "gsl_matrix_isnonneg" '(*) int) mtx)))
(define (mtx-equal? mtx1 mtx2)
  (positive? ((foreign-fn "gsl_matrix_equal" '(* *) int) mtx1 mtx2)))

;; Aggregates

(define mtx-min (foreign-fn "gsl_matrix_min" '(*) double))
(define (mtx-min-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn "gsl_matrix_min_index" '(* * *) void)
     mtx row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
(define mtx-max (foreign-fn "gsl_matrix_max" '(*) double))
(define (mtx-max-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn "gsl_matrix_max_index" '(* * *) void)
     mtx row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
;; FIXME: Segfaults.
;; (define mtx-norm1 (foreign-fn "gsl_matrix_norm1" '(*) double))

;; TODO: Views

;; Destructive matrix<->vector copying.

(define (mtx-row->vec! mtx row vec)
  "Copy the ROW-th row of MTX to VEC."
  ((foreign-fn "gsl_matrix_get_row" `(* * ,size_t) int)
   vec mtx row))
(define (mtx-column->vec! mtx column vec)
  "Copy the COLUMN-th column of MTX to VEC."
  ((foreign-fn "gsl_matrix_get_col" `(* * ,size_t) int)
   vec mtx column))

(define (vec->mtx-row! vec mtx row)
  "Copy the VEC to ROW-th row of MTX."
  ((foreign-fn "gsl_matrix_set_row" `(* ,size_t *))
   mtx row vec))
(define (vec->mtx-column! vec mtx column)
  "Copy the VEC to COLUMN-th column of MTX."
  ((foreign-fn "gsl_matrix_set_col" `(* ,size_t *))
   mtx column vec))

;; Row and column operations

(define* (mtx-transpose! mtx #:optional (dest #t))
  "Transpose the MTX depending on DEST value.
DEST might be one of:
- Pointer: transpose the MTX into the pointed-to matrix
- #t: Create a new matrix and put the transposed MTX there.
- #f: Modify the MTX in-place."
  (let ((real-dest (cond
                    ((eq? dest #t)   (mtx-alloc (mtx-columns mtx) (mtx-rows mtx)))
                    ((eq? dest #f)   (if (= (mtx-columns mtx) (mtx-rows mtx))
                                         mtx
                                         (error "Cannot transpose a non-square matrix in place!")))
                    ((pointer? dest) dest)
                    (else (error "Cannot copy transposed matrix into " dest)))))
    (let row-rec ((row 0))
      (when (< row (mtx-rows mtx))
        (let column-rec ((column 0))
          (when (< column (mtx-columns mtx))
            (mtx-set! real-dest column row (mtx-get mtx row column))
            (column-rec (1+ column))))
        (row-rec (1+ row))))
    ;; FIXME: Segfaults.
    ;; ((foreign-fn "gsl_matrix_transpose_memcpy" '(* *) int)
    ;;  real-dest mtx)
    real-dest))

;; Math operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (mtx arg)
    (let ((new (mtx-copy mtx)))
      (op new arg)
      new)))

(define mtx-add! (foreign-fn "gsl_matrix_add" '(* *) int))
(define mtx-add (act-on-copy mtx-add!))

(define mtx-subtract! (foreign-fn "gsl_matrix_sub" '(* *) int))
(define mtx-subtract (act-on-copy mtx-subtract!))

(define mtx-multiply-elements! (foreign-fn "gsl_matrix_mul_elements" '(* *) int))
(define mtx-multiply-elements (act-on-copy mtx-multiply-elements!))

(define mtx-divide-elements! (foreign-fn "gsl_matrix_div_elements" '(* *) int))
(define mtx-divide-elements (act-on-copy mtx-divide-elements!))

(define mtx-scale! (foreign-fn "gsl_matrix_scale" `(* ,double) int))
(define mtx-scale (act-on-copy mtx-scale!))

(define mtx-scale-columns! (foreign-fn "gsl_matrix_scale_columns" '(* *) int))
(define mtx-scale-columns (act-on-copy mtx-scale-columns!))

(define mtx-scale-rows! (foreign-fn "gsl_matrix_scale_rows" '(* *) int))
(define mtx-scale-rows (act-on-copy mtx-scale-rows!))

(define mtx-add-constant! (foreign-fn "gsl_matrix_add_constant" '(* *) int))
(define mtx-add-constant (act-on-copy mtx-add-constant!))

;; TODO: call-with-mtx-copy
(define (call-with-mtx rows columns fill thunk)
  "Call THUNK on a new ROWxCOLUMNS FILL-ed matrix.
Free the matrix afterwards."
  (let* ((mtx (mtx-alloc rows columns fill))
         (result (thunk mtx)))
    (mtx-free mtx)
    result))

(define-syntax-rule (with-mtx (mtx rows columns fill) body ...)
  "Run BODY with MTX bound to ROWSxCOLUMNS matrix FILLed with data."
  (call-with-mtx
   rows columns fill
   (lambda (mtx)
     body ...)))

(define (for-mtx thunk mtx)
  "Call THUNK with every (ROW COLUMN VALUE) of MTX."
  (let row-rec ((row 0))
    (when (< row (mtx-rows mtx))
      (let column-rec ((column 0))
        (when (< column (mtx-columns mtx))
          (thunk row column (mtx-get mtx row column))
          (column-rec (1+ column))))
      (row-rec (1+ row)))))

(define (ensure-gsl thing)
  "Turn THING into a GSL-friendly object:
- Pointer: do nothing.
- Vector/list of numbers: turn into a gsl_vector.
- Vector/list of vectors/lists: turn into a gsl_matrix."
  (cond
   ((pointer? thing)
    thing)
   ((and (sequence? thing)
         (sequence? (sequence-ref thing 0)))
    (mtx-alloc (sequence-length thing)
               (sequence-length (sequence-ref thing 0))
               thing))
   ((sequence? thing)
    (vec-alloc (sequence-length thing) thing))
   (else
    (error "Cannot convert to GSL vector/matrix: " thing))))
