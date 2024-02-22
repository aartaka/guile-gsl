(define-module (gsl matrices)
  #:use-module (gsl core)
  #:use-module (gsl vectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43))

;; Access
(define (mtx-dimensions mtx)
  (parse-c-struct mtx (list size_t size_t)))
(define (mtx-rows mtx)
  (first (mtx-dimensions mtx)))
(define (mtx-columns mtx)
  (second (mtx-dimensions mtx)))

(define (bound-check-matrix mtx row column)
  (unless (and (< row (mtx-rows mtx))
               (< column (mtx-columns mtx)))
    (error (format #f "Trying to index matrix ~s (of size ~dx~d) with out-of-bounds indices ~dx~d" mtx (mtx-rows mtx) (mtx-columns mtx) row column))))

(define (mtx-get mtx row column)
  (bound-check-matrix mtx row column)
  ((foreign-fn "gsl_matrix_get" `(* ,size_t ,size_t) double)
   mtx row column))
(define (mtx-set! mtx row column val)
  (bound-check-matrix mtx row column)
  ((foreign-fn "gsl_matrix_set" `(* ,size_t ,size_t ,double) void)
   mtx row column val))
(define (mtx-ptr mtx row column)
  (bound-check-matrix mtx row column)
  ((foreign-fn "gsl_matrix_ptr" `(* ,size_t ,size_t) '*)
   mtx row column))

;; (De)allocation
(define* (mtx-alloc rows columns #:optional (fill #f))
  "Create a new ROWSxCOLUMNS gsl_matrix.
FILL might be one of:
- #f for uninitialized matrix (garbage values, use `mtx-calloc' for
  zero-initialized or numeric FILL for constant-initialized matrices).
- Real number to fill the vector with the same double value."
  (let ((mtx ((foreign-fn "gsl_matrix_alloc" (list size_t size_t) '*)
              rows columns)))
    (when fill
      (cond
       ((number? fill)
        ((foreign-fn "gsl_matrix_set_all" `(* ,double) void)
         mtx fill))
       ((vector? fill)
        (vector-map
         (lambda (row vec)
           (vector-map
            (lambda (column elem)
              (mtx-set! mtx row column elem))
            vec))
         fill))
       (else
        (error "Don't know how to fill a matrix with" fill))))
    mtx))
(define mtx-calloc
  (foreign-fn "gsl_matrix_calloc" (list size_t size_t) '*))
(define mtx-free
  (foreign-fn "gsl_matrix_free" '(*) void))

(define (mtx-copy! src dest)
  ((foreign-fn "gsl_matrix_memcpy" '(* *) int)
   dest src))
(define (mtx-copy src)
  (let ((new-mtx (mtx-alloc (mtx-rows src) (mtx-columns src))))
    (mtx-copy! src new-mtx)
    new-mtx))

(define (mtx->2d-vector mtx)
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
(define mtx-min-index (foreign-fn "gsl_matrix_min_index" '(*) double))
(define mtx-max (foreign-fn "gsl_matrix_max" '(*) double))
(define mtx-max-index (foreign-fn "gsl_matrix_max_index" '(*) double))
(define mtx-norm1 (foreign-fn "gsl_matrix_norm1" '(*) double))

;; TODO: Views

;; Row and column views

(define (mtx-row mtx row)
  "Return a gsl_vector_view for Nth row of MTX."
  (bound-check-matrix mtx row 0)
  ((foreign-fn "gsl_matrix_row" `(* ,size_t) '*) mtx row))
(define (mtx-column mtx column)
  (bound-check-matrix mtx 0 column)
  "Return a gsl_vector_view for Nth column of MTX."
  ((foreign-fn "gsl_matrix_column" `(* ,size_t) '*) mtx column))

(define (mtx-diagonal mtx)
  ((foreign-fn "gsl_matrix_diagonal" '(*) '*) mtx))
(define (mtx-subdiagonal mtx n)
  "Nth sub-diagonal (below the main diagonal) of MTX."
  ((foreign-fn "gsl_matrix_subdiagonal" `(* ,size_t) '*) mtx n))
(define (mtx-superdiagonal mtx n)
  "Nth super-diagonal (above the main diagonal) of MTX."
  ((foreign-fn "gsl_matrix_superdiagonal" `(* ,size_t) '*) mtx n))

(define (mtx-subrow mtx row offset size)
  "Return a gsl_vector_view for ROWth row of MTX.
The view starts from OFFSETth element of the row and stretches for SIZE elements."
  (bound-check-matrix mtx row 0)
  ((foreign-fn "gsl_matrix_subrow" `(* ,size_t ,size_t ,size_t) '*) mtx row offset size))
(define (mtx-subcolumn mtx column offset size)
  "Return a gsl_vector_view for COLUMNth column of MTX.
The view starts from OFFSETth element of the column and stretches for SIZE elements."
  (bound-check-matrix mtx 0 column)
  ((foreign-fn "gsl_matrix_subcolumn" `(* ,size_t ,size_t ,size_t) '*) mtx column offset size))

;; Destructive matrix<->vector copying.

(define (mtx-row->vec! mtx row vec)
  (bound-check-matrix mtx row 0)
  ((foreign-fn "gsl_matrix_get_row" `(* * ,size_t) int)
   vec mtx row))
(define (mtx-column->vec! mtx column vec)
  (bound-check-matrix mtx 0 column)
  ((foreign-fn "gsl_matrix_get_col" `(* * ,size_t) int)
   vec mtx column))

(define (vec->mtx-row! vec mtx row)
  (bound-check-matrix mtx row 0)
  ((foreign-fn "gsl_matrix_set_row" `(* ,size_t *))
   mtx row vec))
(define (vec->mtx-column! vec mtx column)
  (bound-check-matrix mtx 0 column)
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
    ((foreign-fn "gsl_matrix_transpose_memcpy" '(* *) int) real-dest mtx)
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

(define (call-with-mtx rows columns fill thunk)
  (let* ((mtx (mtx-alloc rows columns fill))
         (result (thunk mtx)))
    (mtx-free mtx)
    result))

(define (ensure-gsl thing)
  (cond
   ((pointer? thing)
    thing)
   ((and (vector? thing)
         (vector? (vector-ref thing 0)))
    (mtx-alloc (vector-length thing)
               (vector-length (vector-ref thing 0))
               thing))
   ((vector? thing)
    (vec-alloc (vector-length thing) thing))))
