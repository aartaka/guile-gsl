(define-module (gsl vectors)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export-syntax (with)
  #:export ( ;; Accessors
            parts
            length
            data
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
            fill!
            ->vector
            ;; Predicates
            null?
            positive?
            negative?
            non-negative?
            equal?
            ;; Aggregate
            sum
            min
            min-index
            max
            max-index
            ;; Ops
            add!
            add
            subtract!
            subtract
            multiply!
            multiply
            divide!
            divide
            scale!
            scale
            add-constant!
            add-constant
            axpby!
            ;; Utils
            call-with-vec
            for-vec
            for-each))

;; Access
(define (parts vec)
  "Return VEC structure parts:
- size (int)
- stride (int)
- data (pointer)
- block (pointer)
- and owner (int)"
  (parse-c-struct vec (list size_t size_t '* '* int)))
(define (length vec)
  "Number of elements in VEC."
  (first (parts vec)))
(define (data vec)
  "Pointer the the actual data (double numbers) residing in VEC."
  (third (parts vec)))

(define %get (foreign-fn "gsl_vector_get" `(* ,size_t) double))
(define (get vec i)
  "Get I-th element VEC."
  (%get vec i))
(define ref get)

(define %set (foreign-fn "gsl_vector_set" `(* ,size_t ,double) void))
(define (set! vec i val)
  "Set I-th element in VEC to VAL."
  (%set vec i val))

(define (ptr vec i)
  "Get the pointer to the I-th element of VEC."
  ((foreign-fn "gsl_vector_ptr" `(* ,size_t) '*) vec i))

;; Vector (de)allocation
(define* (alloc k #:optional (fill #f))
  "Create a new gsl_vector of size K filled with FILL.
FILL might be one of:
- #f for uninitialized vector (garbage values, use `calloc' for
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
           (when (< idx (length vec))
             (set! vec idx elem)))
         fill))
       (else
        (error "Don't know how to fill a vector with" fill))))
    vec))
(define make alloc)
(define (calloc size)
  "Allocate a 0-initialized vector of size SIZE"
  ((foreign-fn "gsl_vector_calloc" (list size_t) '*) size))
(define free
  (foreign-fn "gsl_vector_free" '(*) void))

(define (fill! vec fill)
  "Fill the VEC with FILL, a number."
  ((foreign-fn "gsl_vector_set_all" `(* ,double) void)
   vec fill))

(define* (copy! src #:optional (dest #t))
  "Copy the SRC vector to DEST.
DEST can be one of:
- #t to create a new vector (default).
- Pointer to copy SRC into it."
  (let ((real-dest (cond
                    ((eq? #t dest)
                     (alloc (length src)))
                    ((pointer? dest)
                     dest)
                    (else
                     (error "Cannot copy the matrix into " dest)))))
    ((foreign-fn "gsl_vector_memcpy" '(* *) int)
     real-dest src)
    real-dest))

(define (swap! a b)
  "Exchange the values between A and B (vectors)."
  ((foreign-fn "gsl_vector_swap" '(* *) int)
   a b))

(define (->vector vec)
  "Convert VEC to a Scheme vector."
  (vector-unfold (lambda (idx) (get vec idx))
                 (length vec)))

;; Predicates
;; FIXME: Maybe call it zero?
(define (null? vec)
  (< 0 ((foreign-fn "gsl_vector_isnull" '(*) int) vec)))
(define (positive? vec)
  (< 0 ((foreign-fn "gsl_vector_ispos" '(*) int) vec)))
(define (negative? vec)
  (< 0 ((foreign-fn "gsl_vector_isneg" '(*) int) vec)))
(define (non-negative? vec)
  (< 0 ((foreign-fn "gsl_vector_isnonneg" '(*) int) vec)))
(define (equal? vec1 vec2)
  (< 0 ((foreign-fn "gsl_vector_equal" '(* *) int) vec1 vec2)))

;; Aggregate functions

(define sum (foreign-fn "gsl_vector_sum" '(*) double))
(define min (foreign-fn "gsl_vector_min" '(*) double))
(define min-index (foreign-fn "gsl_vector_min_index" '(*) size_t))
(define max (foreign-fn "gsl_vector_max" '(*) double))
(define max-index (foreign-fn "gsl_vector_max_index" '(*) size_t))

;; Operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (vec arg)
    (let ((new (copy! vec)))
      (op new arg)
      new)))

(define add! (foreign-fn "gsl_vector_add" '(* *) int))
(define add (act-on-copy add!))

(define subtract! (foreign-fn "gsl_vector_sub" '(* *) int))
(define subtract (act-on-copy subtract!))

(define multiply! (foreign-fn "gsl_vector_mul" '(* *) int))
(define multiply (act-on-copy multiply!))

(define divide! (foreign-fn "gsl_vector_div" '(* *) int))
(define divide (act-on-copy divide!))

(define scale! (foreign-fn "gsl_vector_scale" `(* ,double) int))
(define scale (act-on-copy scale!))

(define add-constant! (foreign-fn "gsl_vector_add_constant" `(* ,double) int))
(define add-constant (act-on-copy add-constant!))

(define (axpby! alpha x beta y)
  "Perform αx + βy and store the result in Y."
  ((foreign-fn "gsl_vector_axpby" `(,double * ,double *) int)
   alpha x beta y))

;; TODO: call-with-copy
(define (call-with-vec size fill thunk)
  "Call THUNK with a new SIZE-d vector FILLed with data.
Free the vector afterwards."
  (let* ((vec (alloc size fill))
         (result (thunk vec)))
    (free vec)
    result))

(define-syntax-rule (with (vec size fill) body ...)
  "Run BODY with VEC bound to SIZE-d vector FILLed with data."
  (call-with-vec
   size fill
   (lambda (vec)
     body ...)))

(define (for-vec thunk vec)
  "Call THUNK with (INDEX VALUE) of every element in VEC."
  (let ((len (length vec)))
    (do ((idx 0 (1+ idx)))
        ((= idx len))
      (thunk idx (get vec idx)))))
(define for-each for-vec)
