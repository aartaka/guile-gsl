(define-module (gsl matrices)
  #:use-module (gsl utils)
  #:use-module ((gsl vectors) #:prefix vec:)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43)
  #:export-syntax (with
                   with-column
                   with-row)
  #:export (;; Wrapping
            wrap
            unwrap
            mtx?
            ;; Access
            type
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
            alloc-square
            free
            copy!
            fill!
            identity!
            ->2d-vector
            ;; Predicates
            null?
            zero?
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
            call-with-row
            call-with-column
            for-mtx
            for-each
            ensure-gsl))

(define-record-type <mtx>
  (wrap pointer type)
  mtx?
  (pointer unwrap)
  (type type))
(set-record-type-printer!
 <mtx>
 (lambda (v p)
   (format p "#<gsl_matrix~a of ~ax~a ~a>"
           (if (eq? 'f32 (type v))
               "_float"
               "")
           (rows v)
           (columns v)
           (cond
            ((null? v) "zeros")
            ((positive? v) "positives")
            ((negative? v) "negatives")
            ((non-negative? v) "nonnegatives")
            (else "nondescripts")))))

(define (dispatch mtx f64 f32)
  (case (type mtx)
    ((f64) f64)
    ((f32) f32)))

;; Access
(define (parts mtx)
  "Return parts of MTX struct:
- rows (int)
- columns (int)
- tda??? (int)
- data (pointer)
- block (pointer)
- owner (int)"
  (parse-c-struct (unwrap mtx) (list size_t size_t size_t '* '* int)))
(define (data mtx)
  "Return raw MTX data array (of doubles/floats) pointer."
  (fourth (parts mtx)))
(define (dimensions mtx)
  "Return a list of (ROWS COLUMNS) in MTX."
  (take (parts mtx) 2))
(define (rows mtx)
  "Return number of rows in MTX."
  (first (dimensions mtx)))
(define (columns mtx)
  "Return number of columns in MTX."
  (second (dimensions mtx)))
(define cols columns)

(define %get (foreign-fn "gsl_matrix_get" `(* ,size_t ,size_t) double))
(define %get-f32 (foreign-fn "gsl_matrix_float_get" `(* ,size_t ,size_t) float))
(define (get mtx row column)
  "Get the element at ROW and COLUMN (0-indexed) in MTX."
  ((dispatch mtx
             %get
             %get-f32)
   (unwrap mtx) row column))
(define ref get)
(define %set (foreign-fn "gsl_matrix_set" `(* ,size_t ,size_t ,double) void))
(define %set-f32 (foreign-fn "gsl_matrix_float_set" `(* ,size_t ,size_t ,float) void))
(define (set! mtx row column val)
  "Set the MTX[ROW][COLUMN]-th element (0-indexed) to VAL."
  ((dispatch mtx
             %set
             %set-f32)
   (unwrap mtx) row column val))
(define (ptr mtx row column)
  "Return a pointer to [ROW][COLUMN]-th element of MTX."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_ptr"
                         "gsl_matrix_float_ptr") `(* ,size_t ,size_t) '*)
   (unwrap mtx) row column))

;; (De)allocation
(define* (alloc n-rows n-columns #:optional (fill #f) (type 'f64))
  "Create a new TYPEd ROWSxCOLUMNS matrix.
FILL might be one of:
- #f or not provided for uninitialized matrix (garbage values, use
  `calloc' for zero-initialized or numeric FILL for constant-initialized
  matrices).
- Real number to fill the matrix with the same double value.
- A list/vector of lists/vectors with numbers to fill in.
- Or another allocated matrix to copy its contents."
  (let ((mtx (wrap ((foreign-fn (case type
                                  ((f64) "gsl_matrix_alloc")
                                  ((f32) "gsl_matrix_alloc"))
                                (list size_t size_t) '*)
                    n-rows n-columns)
                   type)))
    (when fill
      (cond
       ((number? fill)
        ((dispatch mtx
                   (foreign-fn "gsl_matrix_set_all" `(* ,double) void)
                   (foreign-fn "gsl_matrix_float_set_all" `(* ,float) void))
         (unwrap mtx) fill))
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
       ((mtx? fill)
        (copy! fill mtx))
       (else
        (error "Don't know how to fill a matrix with" fill))))
    mtx))
(define make alloc)
(define* (calloc row columns #:optional (type 'f64))
  "Allocate a new TYPEd matrix of ROWxCOLUMNS initialized to 0."
  (wrap ((foreign-fn (case type
                       ((f64) "gsl_matrix_calloc")
                       ((f32) "gsl_matrix_float_calloc")) (list size_t size_t) '*)
         row columns)
        type))
(define* (alloc-square size #:optional (fill #f) (type 'f64))
  "Allocate a square matrix with rows == columns == SIZE.
FILL and TYPE as per `alloc'."
  (alloc size size fill type))
(define (free mtx . mtxs)
  (let ((fn (foreign-fn (dispatch mtx
                                  "gsl_matrix_free"
                                  "gsl_matrix_float_free") '(*) void)))
    (map (compose fn unwrap) (cons mtx mtxs))))

(define (fill! mtx fill)
  "Set all elements of MTX to FILL."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_set_all"
                         "gsl_matrix_float_set_all") `(* ,double) void)
   (unwrap mtx) fill))
(define* (identity! #:optional mtx rows columns)
  "Turn MTX into an identity matrix.
(All zeros except ones on the main diagonal.)
In case MTX if #f, create a new one of ROWSxCOLUMNS.
In either case, return resultant identity matrix."
  (let ((mtx (or mtx (alloc rows columns))))
    ((foreign-fn (dispatch mtx
                           "gsl_matrix_set_identity"
                           "gsl_matrix_float_set_identity") `(*) void)
     (unwrap mtx))
    mtx))

(define* (copy! src #:optional (dest #t))
  "Copy the SRC matrix to DEST and return DEST.
DEST can be one of:
- #t to create a new matrix (default) and return it.
- Pointer/matrix to copy the SRC into it."
  (let ((real-dest (cond
                    ((eq? #t dest)
                     (unwrap
                      (alloc (rows src) (columns src) 0 (type src))))
                    ((pointer? dest)
                     dest)
                    ((mtx? dest)
                     (unwrap dest))
                    (else
                     (error "Cannot copy the matrix into " dest)))))
    ((foreign-fn (dispatch src
                           "gsl_matrix_memcpy"
                           "gsl_matrix_float_memcpy") '(* *) int)
     real-dest (unwrap src))
    (wrap real-dest (type src))))

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
(define (null? mtx)
  "Are all elements in MTX zero?"
  (< 0 ((foreign-fn (dispatch mtx
                              "gsl_matrix_isnull"
                              "gsl_matrix_float_isnull") '(*) int)
        (unwrap mtx))))
(define zero? null?)
(define (positive? mtx)
  (< 0 ((foreign-fn (dispatch mtx
                              "gsl_matrix_ispos"
                              "gsl_matrix_float_ispos") '(*) int)
        (unwrap mtx))))
(define (negative? mtx)
  (< 0 ((foreign-fn (dispatch mtx
                              "gsl_matrix_isneg"
                              "gsl_matrix_float_isneg") '(*) int)
        (unwrap mtx))))
(define (non-negative? mtx)
  (< 0 ((foreign-fn (dispatch mtx
                              "gsl_matrix_isnonneg"
                              "gsl_matrix_float_isnonneg") '(*) int)
        (unwrap mtx))))
(define (equal? mtx1 mtx2)
  "Check whether MTX1 and MTX2 are per-element equal."
  (< 0 ((foreign-fn (dispatch mtx1
                              "gsl_matrix_equal"
                              "gsl_matrix_float_equal") '(* *) int)
        (unwrap mtx1) (unwrap mtx2))))

;; Aggregates

(define (min mtx)
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_min"
                         "gsl_matrix_float_min") '(*) double)
   (unwrap mtx)))
(define (min-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn (dispatch mtx
                           "gsl_matrix_min_index"
                           "gsl_matrix_float_min_index") '(* * *) void)
     (unwrap mtx) row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
(define (max mtx)
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_max"
                         "gsl_matrix_float_max") '(*) double)
   (unwrap mtx)))
(define (max-index mtx)
  (let ((row (make-c-ptr size_t))
        (column (make-c-ptr size_t)))
    ((foreign-fn (dispatch mtx
                           "gsl_matrix_max_index"
                           "gsl_matrix_float_max_index") '(* * *) void)
     (unwrap mtx) row column)
    (append (parse-c-struct row (list size_t))
            (parse-c-struct column (list size_t)))))
(define (norm1 mtx)
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_norm1"
                         "gsl_matrix_float_norm1") '(*) double)
   (unwrap mtx)))

;; Destructive matrix<->vector copying.

(define* (row->vec! mtx row #:optional (vec (vec:alloc (columns mtx) 0 (type mtx))))
  "Copy the ROW-th row of MTX to VEC (create if not provided.)"
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_get_row"
                         "gsl_matrix_float_get_row") `(* * ,size_t) int)
   (vec:unwrap vec) (unwrap mtx) row)
  vec)
(define* (column->vec! mtx column #:optional (vec (vec:alloc (rows mtx) 0 (type mtx))))
  "Copy the COLUMN-th column of MTX to VEC (create if not provided.)"
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_get_col"
                         "gsl_matrix_float_get_col") `(* * ,size_t) int)
   (vec:unwrap vec) (unwrap mtx) column)
  vec)

(define (vec->row! vec mtx row)
  "Copy the VEC to ROW-th row of MTX."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_set_row"
                         "gsl_matrix_float_set_row") `(* ,size_t *))
   (unwrap mtx) row (vec:unwrap vec)))
(define (vec->column! vec mtx column)
  "Copy the VEC to COLUMN-th column of MTX."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_set_col"
                         "gsl_matrix_float_set_col") `(* ,size_t *))
   (unwrap mtx) column (vec:unwrap vec)))

;; Row and column operations

(define* (transpose! mtx #:optional (dest #t))
  "Transpose the MTX depending on DEST value and return DEST.
DEST might be one of:
- Pointer/matrix: transpose the MTX into the pointed-to matrix
- #t: Create a new matrix and put the transposed MTX there.
- #f: Modify the MTX in-place."
  (let ((real-dest (cond
                    ((eq? dest #t)   (unwrap
                                      (alloc (columns mtx) (rows mtx) 0 (type mtx))))
                    ((eq? dest #f)   (unwrap
                                      (if (= (columns mtx) (rows mtx))
                                          mtx
                                          (error "Cannot transpose a non-square matrix in place!"))))
                    ((mtx? dest)     (unwrap dest))
                    ((pointer? dest) dest)
                    (else (error "Cannot copy transposed matrix into " dest)))))
    ((foreign-fn (dispatch mtx
                           "gsl_matrix_transpose_memcpy"
                           "gsl_matrix_float_transpose_memcpy") '(* *) int)
     real-dest (unwrap mtx))
    (wrap real-dest (type mtx))))

;; Math operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (mtx arg)
    (let ((new (copy! mtx)))
      (op new arg)
      new)))

(define (add! mtx1 mtx2)
  "This function adds the elements of matrix MTX2 to the elements of MTX1.
The result is stored in MTX1 and MTX2 remains unchanged."
  ((foreign-fn (dispatch mtx1
                         "gsl_matrix_add"
                         "gsl_matrix_float_add") '(* *) int)
   (unwrap mtx1) (unwrap mtx2)))
(define (add mtx1 mtx2)
  "Like `add!', but creates and returns a new matrix for the result."
  ((act-on-copy add!) mtx1 mtx2))

(define (subtract! mtx1 mtx2)
  "This function subtracts the elements of matrix MTX2 from the elements of matrix MTX1.
The result is stored in MTX1 and MTX2 remains unchanged."
  ((foreign-fn (dispatch mtx1
                         "gsl_matrix_sub"
                         "gsl_matrix_float_sub") '(* *) int)
   (unwrap mtx1) (unwrap mtx2)))
(define (subtract mtx1 mtx2)
  "Like `subtract!', but creates and returns a new matrix for the result."
  ((act-on-copy subtract!) mtx1 mtx2))

(define (multiply-elements! mtx1 mtx2)
  "This function multiplies the elements of matrix MTX1 by the elements of matrix MTX2.
The result is stored in MTX1 and MTX2 remains unchanged."
  ((foreign-fn (dispatch mtx1
                         "gsl_matrix_mul_elements"
                         "gsl_matrix_float_mul_elements") '(* *) int)
   (unwrap mtx1) (unwrap mtx2)))
(define (multiply-elements mtx1 mtx2)
  "Like `multiply-elements!', but creates and returns a new matrix for the result."
  ((act-on-copy multiply-elements!) mtx1 mtx2))

(define (divide-elements! mtx1 mtx2)
  "This function divides the elements of matrix MTX1 by the elements of matrix MTX2.
The result is stored in MTX1 and MTX2 remains unchanged."
  ((foreign-fn (dispatch mtx1
                         "gsl_matrix_div_elements"
                         "gsl_matrix_float_div_elements") '(* *) int)
   (unwrap mtx1) (unwrap mtx2)))
(define (divide-elements mtx1 mtx2)
  "Like `divide-elements!', but creates and returns a new matrix for the result."
  ((act-on-copy divide-elements!) mtx1 mtx2))

(define (scale! mtx scalar)
  "This function multiplies the elements of matrix MTX by the SCALAR.
The result is stored in MTX."
  ((dispatch mtx
             (foreign-fn "gsl_matrix_scale" `(* ,double) int)
             (foreign-fn "gsl_matrix_float_scale" `(* ,float) int))
   (unwrap mtx) scalar))
(define (scale mtx scalar)
  "Like `scale!', but creates and returns a new matrix for the result."
  ((act-on-copy scale!) mtx scalar))

(define (scale-columns! mtx vec)
  "This function scales the columns of the M-by-N MTX MTX by the elements of the VEC, of length N.
The j-th column of MTX is multiplied by VEC[j]."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_scale_columns"
                         "gsl_matrix_float_scale_columns") '(* *) int)
   (unwrap mtx) (vec:unwrap vec)))
(define (scale-columns mtx vec)
  "Like `scale-columns!', but creates and returns a new matrix for the result."
  ((act-on-copy scale-columns!) mtx vec))

(define (scale-rows! mtx vec)
  "This function scales the rows of the M-by-N MTX by the elements of the VEC, of length M.
The i-th row of MTX is multiplied by VEC[i]."
  ((foreign-fn (dispatch mtx
                         "gsl_matrix_scale_rows"
                         "gsl_matrix_float_scale_rows") '(* *) int)
   (unwrap mtx) (vec:unwrap vec)))
(define (scale-rows mtx vec)
  "Like `scale-rows!', but creates and returns a new matrix for the result."
  ((act-on-copy scale-rows!) mtx vec))

(define (add-constant! mtx constant)
  "This function adds the CONSTANT the elements of the matrix MTX.
The result is stored in MTX."
  ((dispatch mtx
             (foreign-fn "gsl_matrix_add_constant" `(* ,double) int)
             (foreign-fn "gsl_matrix_float_add_constant" `(* ,float) int))
   (unwrap mtx) constant))
(define (add-constant mtx constant)
  "Like `add-constant!', but creates and returns a new matrix for the result."
  (act-on-copy add-constant!))

;; TODO: call-with-copy
(define* (call-with-mtx rows columns thunk #:optional (fill #f) (type 'f64))
  "Call THUNK on a new ROWxCOLUMNS FILL-ed matrix.
Free the matrix afterwards."
  (let* ((mtx (alloc rows columns fill type)))
    (with-cleanup
     (free mtx)
     (thunk mtx))))

(define-syntax-rule (with (mtx rows columns arg ...) body ...)
  "Run BODY with MTX bound to ROWSxCOLUMNS matrix initialized with ARGS."
  (call-with-mtx
   rows columns
   (lambda (mtx)
     body ...)
   arg ...))

(define* (call-with-row mtx row thunk)
  "Call THUNK with a temporary vector created from ROWth row of MTX."
  (let* ((vec (row->vec! mtx row)))
    (with-cleanup
     (vec:free vec)
     (thunk vec))))
(define-syntax-rule (with-row (vec mtx row) body ...)
  "Run BODY with VEC bound to the ROWth row of MTX."
  (call-with-row
   mtx row
   (lambda (vec)
     body ...)))

(define* (call-with-column mtx column thunk)
  "Call THUNK with a temporary vector created from MTX COLUMNth column."
  (let* ((vec (column->vec! mtx column)))
    (with-cleanup
     (vec:free vec)
     (thunk vec))))
(define-syntax-rule (with-column (vec mtx column) body ...)
  "Run BODY with VEC bound to the COLUMNth column of MTX."
  (call-with-column
   mtx column
   (lambda (vec)
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
- Matrix/vector: do nothing.
- Vector/list of numbers: turn into a gsl_vector.
- Vector/list of vectors/lists: turn into a gsl_matrix."
  (cond
   ((or (mtx? thing)
        (vec:vec? thing))
    thing)
   ((and (sequence? thing)
         (sequence? (sequence-ref thing 0)))
    (alloc (sequence-length thing)
           (sequence-length (sequence-ref thing 0))
           thing))
   ((sequence? thing)
    (vec:alloc (sequence-length thing) thing))
   (else
    (error "Cannot convert to GSL vector/matrix: " thing))))
