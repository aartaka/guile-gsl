(define-module (gsl vectors)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43))

;; Access
(define (vec-size vec)
  (first (parse-c-struct vec (list size_t))))

(define (bound-check-vector vec i)
  (unless (< i (vec-size vec))
    (error (format #f "Trying to index vector ~s (of size ~d) with out-of-bounds index ~d" vec (vec-size vec) i))))

(define (vec-get vec i)
  (bound-check-vector vec i)
  ((foreign-fn "gsl_vector_get" `(* ,size_t) double) vec i))

(define (vec-set! vec i val)
  (bound-check-vector vec i)
  ((foreign-fn "gsl_vector_set" `(* ,size_t ,double) void) vec i val))

(define (vec-ptr vec i)
  (bound-check-vector vec i)
  "Get the pointer to the I-th element of VEC."
  ((foreign-fn "gsl_vector_ptr" `(* ,size_t) '*) vec i))

;; Vector (de)allocation
(define* (vec-alloc k #:optional (fill #f))
  "Create a new gsl_vector of size K filled with FILL.
FILL might be one of:
- #f for uninitialized vector (garbage values, use `vec-calloc' for
  zero-initialized or numeric FILL for constant-initialized vector).
- Real number to fill the vector with the same double value.
- Scheme vector of real numbers."
  (let ((vec ((foreign-fn "gsl_vector_alloc" (list size_t) '*) k)))
    (when fill
      (cond
       ((number? fill)
        ((foreign-fn "gsl_vector_set_all" `(* ,double) void)
         vec fill))
       ((vector? fill)
        (vector-map
         (lambda (idx elem)
           (vec-set! vec idx elem))
         fill))
       (else
        (error "Don't know how to fill a vector with" fill))))
    vec))
(define vec-calloc
  (foreign-fn "gsl_vector_calloc" (list size_t) '*))
(define vec-free
  (foreign-fn "gsl_vector_free" '(*) void))

(define (vec-copy! src dest)
  ((foreign-fn "gsl_vector_memcpy" '(* *) int)
   dest src))
(define (vec-copy src)
  (let ((new-vec (vec-alloc (vec-size src))))
    (vec-copy! src new-vec)
    new-vec))

(define (vec->vector vec)
  (vector-unfold (lambda (idx) (vec-get vec idx))
                 (vec-size vec)))

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

;; Views
;; TODO: Stride
(define (vec-view vec offset count)
  (bound-check-vector vec (- (+ offset count) 1))
  ((foreign-fn "gsl_vector_subvector" `(* ,size_t ,size_t) '*)
   vec offset count))

(define (vec-view-reals vec)
  "Return a vector view of the real parts of the complex vector VEC."
  ((foreign-fn "gsl_vector_complex_real" '(*) '*) vec))

(define (vec-view-imags vec)
  "Return a vector view of the imaginary parts of the complex vector VEC."
  ((foreign-fn "gsl_vector_complex_imag" '(*) '*) vec))

(define (vector->vec-view v)
  "Return a vector view for Scheme vector (of real numbers) V."
  ((foreign-fn "gsl_vector_view_array" `(* ,size_t) '*)
   (make-c-struct (make-list (vector-length v) double)
                  (vector->list v))
   (vector-length v)))

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

(define vec-scale! (foreign-fn "gsl_vector_scale" `(* ,double) int))
(define vec-scale (act-on-copy vec-scale!))

(define vec-add-constant! (foreign-fn "gsl_vector_add_constant" `(* ,double) int))
(define vec-add-constant (act-on-copy vec-add-constant!))

(define (vec-axpby! alpha x beta y)
  "Perform αx + βy and store the result in Y."
  ((foreign-fn "gsl_vector_axpby" `(,double * ,double *) int)
   alpha x beta y))
