(define-module (gsl matrices)
  #:use-module (gsl core)
  #:use-module ((gsl vectors) #:prefix vec-)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export-syntax (with)
  #:export ( ;; Access
            parts
            data
            dimensions
            rows
            columns
            cols
            get
            ref
            set!
            ptr
            ;; (De)allocation
            alloc
            make
            calloc
            free
            copy!
            ->2d-vector
            ;; Predicates
            null?
            positive?
            negative?
            non-negative?
            equal?
            ;; Aggregates
            min
            min-index
            max
            max-index
            norm1
            ;; Matrix<->vector
            row->vec!
            column->vec!
            vec->row!
            vec->column!
            ;; Row/column ops.
            transpose!
            ;; Ops
            add!
            add
            subtract!
            subtract
            multiply-elements!
            multiply-elements
            divide-elements!
            divide-elements
            scale!
            scale
            scale-columns!
            scale-columns
            scale-rows!
            scale-rows
            add-constant!
            add-constant
            ;; Helpers
            call-with-mtx
            for-mtx
            for-each
            ensure-gsl))

;; Access
(define (parts mtx)
  "Return parts of MTX struct:
- rows (int)
- columns (int)
- tda??? (int)
- data (pointer)
- block (pointer)
- owner (int)"
  (parse-c-struct mtx (list size_t size_t size_t '* '* int)))
(define (data mtx)
  "Return raw MTX data array (of doubles) pointer."
  (fourth (parts mtx)))
(define (dimensions mtx)
  "Return a list of (ROWS COLUMNS) in MTX."
  (take (parts mtx) 2))
(define (rows mtx)
  (first (dimensions mtx)))
(define (columns mtx)
  (second (dimensions mtx)))
(define cols columns)

(define %get (foreign-fn "gsl_matrix_get" `(* ,size_t ,size_t) double))
(define (get mtx row column)
  (%get mtx row column))
(define ref get)
(define (set! mtx row column val)
  ((foreign-fn "gsl_matrix_set" `(* ,size_t ,size_t ,double) void)
   mtx row column val))
(define (ptr mtx row column)
  "Return a pointer to [ROW][COLUMN]-th element of MTX."
  ((foreign-fn "gsl_matrix_ptr" `(* ,size_t ,size_t) '*)
   mtx row column))

;; (De)allocation
(define* (alloc n-rows n-columns #:optional (fill #f))
  "Create a new ROWSxCOLUMNS gsl_matrix.
FILL might be one of:
- #f for uninitialized matrix (garbage values, use `calloc' for
  zero-initialized or numeric FILL for constant-initialized matrices).
- Real number to fill the matrix with the same double value.
- Or a list/vector of lists/vectors with numbers to fill in."
  (let ((mtx ((foreign-fn "gsl_matrix_alloc" (list size_t size_t) '*)
              n-rows n-columns)))
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
              (when (and (< row (rows mtx))
                         (< column (columns mtx)))
                (set! mtx row column elem)))
            vec))
         fill))
       (else
        (error "Don't know how to fill a matrix with" fill))))
    mtx))
(define make alloc)
(define (calloc row columns)
  "Allocate a new matrix of ROWxCOLUMNS initialized to 0."
  ((foreign-fn "gsl_matrix_calloc" (list size_t size_t) '*)
   row columns))
(define free
  (foreign-fn "gsl_matrix_free" '(*) void))

(define (fill! mtx fill)
  ((foreign-fn "gsl_matrix_set_all" `(* ,double) void)
   mtx fill))

(define* (copy! src #:optional (dest #t))
  "Copy the SRC matrix to DEST.
DEST can be one of:
- #t to create a new matrix (default).
- Pointer to copy the SRC into it."
  (let ((real-dest (cond
                    ((eq? #t dest)
                     (alloc (rows src) (columns src)))
                    ((pointer? dest)
                     dest)
                    (else
                     (error "Cannot copy the matrix into " dest)))))
    ((foreign-fn "gsl_matrix_memcpy" '(* *) int)
     real-dest src)
    real-dest))

(define (->2d-vector mtx)
  "Convert MTX to a Scheme vector of vectors of numbers."
  (vector-unfold
   (lambda (row)
     (vector-unfold
      (lambda (column)
        (get mtx row column))
      (columns mtx)))
   (rows mtx)))

;; Predicates
;; FIXME: Maybe call it zero?
(define (null? mtx)
  (< 0 ((foreign-fn "gsl_matrix_isnull" '(*) int) mtx)))
(define (positive? mtx)
  (< 0 ((foreign-fn "gsl_matrix_ispos" '(*) int) mtx)))
(define (negative? mtx)
  (< 0 ((foreign-fn "gsl_matrix_isneg" '(*) int) mtx)))
(define (non-negative? mtx)
  (< 0 ((foreign-fn "gsl_matrix_isnonneg" '(*) int) mtx)))
(define (equal? mtx1 mtx2)
  (< 0 ((foreign-fn "gsl_matrix_equal" '(* *) int) mtx1 mtx2)))

;; Aggregates

(define min (foreign-fn "gsl_matrix_min" '(*) double))
(define (min-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn "gsl_matrix_min_index" '(* * *) void)
     mtx row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
(define max (foreign-fn "gsl_matrix_max" '(*) double))
(define (max-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn "gsl_matrix_max_index" '(* * *) void)
     mtx row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
(define norm1 (foreign-fn "gsl_matrix_norm1" '(*) double))

;; Destructive matrix<->vector copying.

(define* (row->vec! mtx row #:optional (vec (vec-alloc (columns mtx))))
  "Copy the ROW-th row of MTX to VEC (create if not provided.)"
  ((foreign-fn "gsl_matrix_get_row" `(* * ,size_t) int)
   vec mtx row)
  vec)
(define* (column->vec! mtx column #:optional (vec (vec-alloc (rows mtx))))
  "Copy the COLUMN-th column of MTX to VEC (create if not provided.)"
  ((foreign-fn "gsl_matrix_get_col" `(* * ,size_t) int)
   vec mtx column)
  vec)

(define (vec->row! vec mtx row)
  "Copy the VEC to ROW-th row of MTX."
  ((foreign-fn "gsl_matrix_set_row" `(* ,size_t *))
   mtx row vec))
(define (vec->column! vec mtx column)
  "Copy the VEC to COLUMN-th column of MTX."
  ((foreign-fn "gsl_matrix_set_col" `(* ,size_t *))
   mtx column vec))

;; Row and column operations

(define* (transpose! mtx #:optional (dest #t))
  "Transpose the MTX depending on DEST value.
DEST might be one of:
- Pointer: transpose the MTX into the pointed-to matrix
- #t: Create a new matrix and put the transposed MTX there.
- #f: Modify the MTX in-place."
  (let ((real-dest (cond
                    ((eq? dest #t)   (alloc (columns mtx) (rows mtx)))
                    ((eq? dest #f)   (if (= (columns mtx) (rows mtx))
                                         mtx
                                         (error "Cannot transpose a non-square matrix in place!")))
                    ((pointer? dest) dest)
                    (else (error "Cannot copy transposed matrix into " dest)))))
    ((foreign-fn "gsl_matrix_transpose_memcpy" '(* *) int)
     real-dest mtx)
    real-dest))

;; Math operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (mtx arg)
    (let ((new (copy! mtx)))
      (op new arg)
      new)))

(define add! (foreign-fn "gsl_matrix_add" '(* *) int))
(define add (act-on-copy add!))

(define subtract! (foreign-fn "gsl_matrix_sub" '(* *) int))
(define subtract (act-on-copy subtract!))

(define multiply-elements! (foreign-fn "gsl_matrix_mul_elements" '(* *) int))
(define multiply-elements (act-on-copy multiply-elements!))

(define divide-elements! (foreign-fn "gsl_matrix_div_elements" '(* *) int))
(define divide-elements (act-on-copy divide-elements!))

(define scale! (foreign-fn "gsl_matrix_scale" `(* ,double) int))
(define scale (act-on-copy scale!))

(define scale-columns! (foreign-fn "gsl_matrix_scale_columns" '(* *) int))
(define scale-columns (act-on-copy scale-columns!))

(define scale-rows! (foreign-fn "gsl_matrix_scale_rows" '(* *) int))
(define scale-rows (act-on-copy scale-rows!))

(define add-constant! (foreign-fn "gsl_matrix_add_constant" `(* ,double) int))
(define add-constant (act-on-copy add-constant!))

;; TODO: call-with-copy
(define (call-with-mtx rows columns fill thunk)
  "Call THUNK on a new ROWxCOLUMNS FILL-ed matrix.
Free the matrix afterwards."
  (let* ((mtx (alloc rows columns fill))
         (result (thunk mtx)))
    (free mtx)
    result))

(define-syntax-rule (with (mtx rows columns fill) body ...)
  "Run BODY with MTX bound to ROWSxCOLUMNS matrix FILLed with data."
  (call-with-mtx
   rows columns fill
   (lambda (mtx)
     body ...)))

(define (for-mtx thunk mtx)
  "Call THUNK with every (ROW COLUMN VALUE) of MTX."
  (let ((rows (rows mtx))
        (columns (columns mtx)))
    (do ((row 0 (1+ row)))
        ((= row rows))
      (do ((column 0 (1+ column)))
          ((= column columns))
        (thunk row column (get mtx row column))))))
(define for-each for-mtx)

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
    (alloc (sequence-length thing)
           (sequence-length (sequence-ref thing 0))
           thing))
   ((sequence? thing)
    (vec-alloc (sequence-length thing) thing))
   (else
    (error "Cannot convert to GSL vector/matrix: " thing))))
