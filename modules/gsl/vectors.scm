(define-module (gsl vectors)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (vec-length
            ;; Accessors
            vec-get
            vec-set!
            vec-ptr
            ;; (De)allocation
            vec-alloc
            vec-calloc
            vec-free
            vec-copy!
            vec-copy
            vec->vector
            ;; Predicates
            vec-null?
            vec-positive?
            vec-negative?
            vec-non-negative?
            vec-equal?
            ;; Aggregate
            vec-sum
            vec-min
            vec-min-index
            vec-max
            vec-max-index
            ;; Views
            vec-view
            vec-view-reals
            vec-view-imags
            ;; Ops
            vec-add!
            vec-add
            vec-subtract!
            vec-subtract
            vec-multiply!
            vec-multiply
            vec-divide!
            vec-divide
            vec-scale!
            vec-scale
            vec-add-constant!
            vec-add-constant
            vec-axpby!
            ;; Utils
            call-with-vec
            for-vec))

;; Access
(define (vec-length vec)
  (first (parse-c-struct vec (list size_t))))

(define (vec-get vec i)
  ((foreign-fn "gsl_vector_get" `(* ,size_t) double) vec i))

(define (vec-set! vec i val)
  ((foreign-fn "gsl_vector_set" `(* ,size_t ,double) void) vec i val))

(define (vec-ptr vec i)
  "Get the pointer to the I-th element of VEC."
  ((foreign-fn "gsl_vector_ptr" `(* ,size_t) '*) vec i))

;; Vector (de)allocation
(define* (vec-alloc k #:optional (fill #f))
  "Create a new gsl_vector of size K filled with FILL.
FILL might be one of:
- #f for uninitialized vector (garbage values, use `vec-calloc' for
  zero-initialized or numeric FILL for constant-initialized vector).
- Real number to fill the vector with the same double value.
- Scheme vector/list of real numbers."
  (let ((vec ((foreign-fn "gsl_vector_alloc" (list size_t) '*) k)))
    (when fill
      (cond
       ((number? fill)
        ((foreign-fn "gsl_vector_set_all" `(* ,double) void)
         vec fill))
       ((sequence? fill)
        (for-sequence
         (lambda (idx elem)
           (when (< idx (vec-length vec))
             (vec-set! vec idx elem)))
         fill))
       (else
        (error "Don't know how to fill a vector with" fill))))
    vec))
(define vec-calloc
  (foreign-fn "gsl_vector_calloc" (list size_t) '*))
(define vec-free
  (foreign-fn "gsl_vector_free" '(*) void))

(define (vec-fill! vec fill)
  ((foreign-fn "gsl_vector_set_all" `(* ,double) void)
   vec fill))

(define (vec-copy! src dest)
  ;; FIXME: This is the right implementation, but it kills the process
  ;; without any indication of what went wrong.
  ;;
  ;; ((foreign-fn "gsl_vector_memcpy" '(* *) int)
  ;;  dest src)
  (let rec ((idx 0))
    (when (< idx (vec-length src))
      (vec-set! dest idx (vec-get src idx))
      (rec (1+ idx)))))
(define (vec-copy src)
  (let ((new-vec (vec-alloc (vec-length src))))
    (vec-copy! src new-vec)
    new-vec))

(define (vec-swap! a b)
  ((foreign-fn "gsl_vector_swap" '(* *) int)
   a b))

(define (vec->vector vec)
  (vector-unfold (lambda (idx) (vec-get vec idx))
                 (vec-length vec)))

;; Predicates
(define (vec-null? vec)
  (positive? ((foreign-fn "gsl_vector_isnull" '(*) int) vec)))
(define (vec-positive? vec)
  (positive? ((foreign-fn "gsl_vector_ispos" '(*) int) vec)))
(define (vec-negative? vec)
  (positive? ((foreign-fn "gsl_vector_isneg" '(*) int) vec)))
(define (vec-non-negative? vec)
  (positive? ((foreign-fn "gsl_vector_isnonneg" '(*) int) vec)))
(define (vec-equal? vec1 vec2)
  (positive? ((foreign-fn "gsl_vector_equal" '(* *) int) vec1 vec2)))

;; Aggregate functions

(define vec-sum (foreign-fn "gsl_vector_sum" '(*) double))
(define vec-min (foreign-fn "gsl_vector_min" '(*) double))
(define vec-min-index (foreign-fn "gsl_vector_min_index" '(*) double))
(define vec-max (foreign-fn "gsl_vector_max" '(*) double))
(define vec-max-index (foreign-fn "gsl_vector_max_index" '(*) double))

;; TODO: Views (segfault on my machine)

;; Operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (vec arg)
    (let ((new (vec-copy vec)))
      (op new arg)
      new)))

(define vec-add! (foreign-fn "gsl_vector_add" '(* *) int))
(define vec-add (act-on-copy vec-add!))

(define vec-subtract! (foreign-fn "gsl_vector_sub" '(* *) int))
(define vec-subtract (act-on-copy vec-subtract!))

(define vec-multiply! (foreign-fn "gsl_vector_mul" '(* *) int))
(define vec-multiply (act-on-copy vec-multiply!))

(define vec-divide! (foreign-fn "gsl_vector_div" '(* *) int))
(define vec-divide (act-on-copy vec-divide!))

;; FIXME: The naive implementation kills the process silently.
;; (define vec-scale! (foreign-fn "gsl_vector_scale" `(* ,double) int))
(define (vec-scale! vec scalar)
  (let rec ((idx 0))
    (when (< idx (vec-length vec))
      (vec-set! vec idx (* scalar (vec-get vec idx)))
      (rec (1+ idx)))))
(define vec-scale (act-on-copy vec-scale!))

(define vec-add-constant! (foreign-fn "gsl_vector_add_constant" `(* ,double) int))
(define vec-add-constant (act-on-copy vec-add-constant!))

(define (vec-axpby! alpha x beta y)
  "Perform αx + βy and store the result in Y."
  ((foreign-fn "gsl_vector_axpby" `(,double * ,double *) int)
   alpha x beta y))

;; TODO: call-with-vec-copy
(define (call-with-vec size fill thunk)
  (let* ((vec (vec-alloc size fill))
         (result (thunk vec)))
    (vec-free vec)
    result))

(define (for-vec thunk vec)
  "Call THUNK with (INDEX VALUE) of every element in VEC."
  (let for ((idx 0))
    (when (< idx (vec-length vec))
      (thunk idx (vec-get vec idx))
      (for (1+ idx)))))
