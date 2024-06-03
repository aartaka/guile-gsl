(define-module (gsl vectors)
  #:use-module (gsl utils)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-43)
  #:export-syntax (with)
  #:export (;; Wrapping
            wrap
            unwrap
            vec?
            ;; Accessors
            type
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
            basis!
            ->vector
            ;; Predicates
            null?
            zero?
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

(define-record-type <vec>
  (wrap pointer type)
  vec?
  (pointer unwrap)
  (type type))
(set-record-type-printer!
 <vec>
 (lambda (v p)
   (format p "#<gsl_vector~a of ~d ~a>"
           (if (eq? 'f32 (type v))
               "_float"
               "")
           (length v)
           (cond
            ((null? v) "zeros")
            ((positive? v) "positives")
            ((negative? v) "negatives")
            ((non-negative? v) "nonnegatives")
            (else "nondescripts")))))

;; Access
(define (parts vec)
  "Return VEC structure parts:
- size (int)
- stride (int)
- data (pointer)
- block (pointer)
- and owner (int)"
  (parse-c-struct (unwrap vec) (list size_t size_t '* '* int)))
(define (length vec)
  "Number of elements in VEC."
  (first (parts vec)))
(define (data vec)
  "Pointer to the actual data (float/double numbers) residing in VEC."
  (third (parts vec)))

(define (dispatch vec f64 f32)
  (case (type vec)
    ((f64) f64)
    ((f32) f32)))

(define %get (foreign-fn "gsl_vector_get" `(* ,size_t) double))
(define %get-f32 (foreign-fn "gsl_vector_float_get" `(* ,size_t) float))
(define (get vec i)
  "Get I-th element VEC."
  ((dispatch vec %get %get-f32)
   (unwrap vec) i))
(define ref get)

(define %set (foreign-fn "gsl_vector_set" `(* ,size_t ,double) void))
(define %set-f32 (foreign-fn "gsl_vector_float_set" `(* ,size_t ,float) void))
(define (set! vec i val)
  "Set I-th element in VEC to VAL."
  ((dispatch vec %set %set-f32)
   (unwrap vec) i val))

(define (ptr vec i)
  "Get the pointer to the I-th element of VEC."
  ((foreign-fn
    (dispatch vec
              "gsl_vector_float_ptr"
              "gsl_vector_ptr")
    `(* ,size_t) '*) (unwrap vec) i))

;; Vector (de)allocation
(define* (alloc k #:optional (fill #f) (type 'f64))
  "Create a new TYPEd vector of size K filled with FILL.
FILL might be one of:
- #f for uninitialized vector (garbage values, use `calloc' for
  zero-initialized or numeric FILL for constant-initialized vector).
- Real number to fill the vector with the same double value.
- Scheme vector/list of real numbers.
- Another allocated vector to copy the contents."
  (let ((vec (wrap ((foreign-fn (case type
                                  ((f64) "gsl_vector_alloc")
                                  ((f32) "gsl_vector_float_alloc")) (list size_t) '*)
                    k)
                   type)))
    (when fill
      (cond
       ((number? fill)
        ((dispatch vec
                   (foreign-fn "gsl_vector_set_all" `(* ,double) void)
                   (foreign-fn "gsl_vector_float_set_all" `(* ,float) void))
         (unwrap vec) fill))
       ((sequence? fill)
        (for-sequence
         (lambda (idx elem)
           (when (< idx (length vec))
             (set! vec idx elem)))
         fill))
       ((vec? fill)
        (copy! fill vec))
       (else
        (error "Don't know how to fill a vector with" fill))))
    vec))
(define make alloc)
(define* (calloc size #:optional (type 'f64))
  "Allocate a 0-initialized TYPEd vector of size SIZE"
  (wrap ((foreign-fn (case type
                       ((f64) "gsl_vector_calloc")
                       ((f32) "gsl_vector_float_calloc"))
                     (list size_t) '*) size)
        type))
(define (free vec . vecs)
  (let ((fn (foreign-fn (dispatch vec
                                  "gsl_vector_free"
                                  "gsl_vector_float_free")
                        '(*) void)))
    (map (compose fn unwrap) (cons vec vecs))))

(define (fill! vec fill)
  "Fill the VEC with FILL, a number."
  ((dispatch vec
             (foreign-fn "gsl_vector_set_all" `(* ,double) void)
             (foreign-fn "gsl_vector_float_set_all" `(* ,float) void))
   (unwrap vec) fill))
(define* (basis! #:optional vec i size)
  "Turn VEC into a basis (all zeros except I-th elem) vector.
If VEC is #f, create a new vector of SIZE and make it a basis.
In either case, return the resultant basis vector."
  (let ((vec (or vec (alloc size))))
    ((foreign-fn
      (dispatch vec
                "gsl_vector_set_basis"
                "gsl_vector_float_set_basis")
      `(* ,size_t) int)
     (unwrap vec) i)
    vec))

(define* (copy! src #:optional (dest #t))
  "Copy the SRC vector to DEST.
DEST can be one of:
- #t to create a new vector (default).
- Pointer to copy SRC into it."
  (let ((real-dest (cond
                    ((eq? #t dest)
                     (unwrap (alloc (length src) 0 (type src))))
                    ((pointer? dest)
                     dest)
                    ((vec? dest)
                     (unwrap dest))
                    (else
                     (error "Cannot copy the vector into " dest)))))
    ((foreign-fn (dispatch src
                           "gsl_vector_memcpy"
                           "gsl_vector_float_memcpy")
                 '(* *) int)
     real-dest (unwrap src))
    (wrap real-dest (type src))))

(define (swap! a b)
  "Exchange the values between A and B (vectors)."
  ((foreign-fn (dispatch a
                         "gsl_vector_swap"
                         "gsl_vector_float_swap")
               '(* *) int)
   (unwrap a) (unwrap b)))

(define (->vector vec)
  "Convert VEC to a Scheme vector."
  (vector-unfold (lambda (idx) (get vec idx))
                 (length vec)))

;; Predicates
(define (null? vec)
  (< 0 ((foreign-fn (dispatch vec
                              "gsl_vector_isnull"
                              "gsl_vector_float_isnull")
                    '(*) int) (unwrap vec))))
(define zero? null?)
(define (positive? vec)
  (< 0 ((foreign-fn (dispatch vec
                              "gsl_vector_ispos"
                              "gsl_vector_float_ispos") '(*) int)
        (unwrap vec))))
(define (negative? vec)
  (< 0 ((foreign-fn (dispatch vec
                              "gsl_vector_isneg"
                              "gsl_vector_float_isneg") '(*) int)
        (unwrap vec))))
(define (non-negative? vec)
  (< 0 ((foreign-fn (dispatch vec
                              "gsl_vector_isnonneg"
                              "gsl_vector_float_isnonneg") '(*) int)
        (unwrap vec))))
(define nonnegative? non-negative?)
(define (equal? vec1 vec2)
  (< 0 ((foreign-fn (dispatch vec1
                              "gsl_vector_equal"
                              "gsl_vector_float_equal") '(* *) int)
        (unwrap vec1) (unwrap vec2))))

;; Aggregate functions

(define (sum vec)
  ((dispatch vec
             (foreign-fn "gsl_vector_sum" '(*) double)
             (foreign-fn "gsl_vector_float_sum" '(*) float))
   (unwrap vec)))
(define (min vec)
  ((dispatch vec
             (foreign-fn "gsl_vector_min" '(*) double)
             (foreign-fn "gsl_vector_float_min" '(*) float))
   (unwrap vec)))
(define (max vec)
  ((dispatch vec
             (foreign-fn "gsl_vector_max" '(*) double)
             (foreign-fn "gsl_vector_float_max" '(*) float))
   (unwrap vec)))
(define (min-index vec)
  ((foreign-fn (dispatch vec
                         "gsl_vector_min_index"
                         "gsl_vector_float_min_index")
               '(*) size_t)
   (unwrap vec)))
(define (max-index vec)
  ((foreign-fn (dispatch vec
                         "gsl_vector_max_index"
                         "gsl_vector_float_max_index")
               '(*) size_t)
   (unwrap vec)))

;; Operations

(define (act-on-copy op)
  "Wrapper to generate the non-destructive versions of GSL ops."
  (lambda (vec arg)
    (let ((new (copy! vec)))
      (op new arg)
      new)))

(define (add! vec1 vec2)
  "This function adds the elements of vector VEC2 to the elements of vector VEC1.
The result is stored in VEC1 and VEC2 remains unchanged. The two
vectors must have the same length."
  ((foreign-fn (dispatch vec1
                         "gsl_vector_add"
                         "gsl_vector_float_add") '(* *) int)
   (unwrap vec1) (unwrap vec2)))
(define (add vec1 vec2)
  "Like `add!', but creates a new vector for the result of the addition.
Both VEC1 and VEC2 are unaffected by the operation."
  ((act-on-copy add!) vec1 vec2))

(define (subtract! vec1 vec2)
  "This function subtracts the elements of vector VEC2 from the elements of VEC1.
The resultis stored in VEC1 and VEC2 remains unchanged.  The two
vectors must have the same length."
  ((foreign-fn (dispatch vec1
                         "gsl_vector_sub"
                         "gsl_vector_float_sub") '(* *) int)
   (unwrap vec1) (unwrap vec2)))
(define (subtract vec1 vec2)
  "Like `subtract!', but creates a new vector for the result of the subtraction.
Both VEC1 and VEC2 are unaffected by the operation."
  ((act-on-copy subtract!) vec1 vec2))

(define (multiply! vec1 vec2)
  "This function multiplies the elements of vector VEC1 by the elements of vector VEC2.
The result is stored in VEC1 and VEC2 remains unchanged. The two
vectors must have the same length."
  ((foreign-fn (dispatch vec1
                         "gsl_vector_mul"
                         "gsl_vector_float_mul") '(* *) int)
   (unwrap vec1) (unwrap vec2)))
(define (multiply vec1 vec2)
  "Like `multiply!', but creates a new vector for the result of the multiplication.
Both VEC1 and VEC2 are unaffected by the operation."
  ((act-on-copy multiply!) vec1 vec2))

(define (divide! vec1 vec2)
  "This function divides the elements of vector VEC1 by the elements of vector VEC2.
The result is stored in VEC1 and VEC2 remains unchanged. The two
vectors must have the same length."
  ((foreign-fn (dispatch vec1
                         "gsl_vector_div"
                         "gsl_vector_float_div") '(* *) int)
   (unwrap vec1) (unwrap vec2)))
(define (divide vec1 vec2)
  "Like `divide!', but creates a new vector for the result of the division.
Both VEC1 and VEC2 are unaffected by the operation."
  ((act-on-copy divide!) vec1 vec2))

(define (scale! vec scale)
  "This function multiplies the elements of VEC by the constant factor SCALE.
 The result is stored in VEC."
  ((dispatch vec
             (foreign-fn "gsl_vector_scale" `(* ,double) int)
             (foreign-fn "gsl_vector_float_scale" `(* ,float) int))
   (unwrap vec) scale))
(define (scale vec scale)
  "Like `scale!', but creates a new vector for the result of the scaling."
  ((act-on-copy scale!) vec scale))

(define (add-constant! vec constant)
  "This function adds the CONSTANT to the elements of the VEC.
The result is stored in VEC"
  ((dispatch vec
             (foreign-fn "gsl_vector_add_constant" `(* ,double) int)
             (foreign-fn "gsl_vector_float_add_constant" `(* ,float) int))
   (unwrap vec) constant))
(define (add-constant vec constant)
  "Like `add-constant!', but creates a new vector for the result of the scaling."
  ((act-on-copy add-constant!) vec constant))

(define (axpby! alpha x beta y)
  "Perform αx + βy and store the result in Y."
  ((dispatch x
             (foreign-fn "gsl_vector_axpby" `(,double * ,double *) int)
             (foreign-fn "gsl_vector_float_axpby" `(,float * ,float *) int))
   (unwrap alpha) x (unwrap beta) y))

;; TODO: call-with-copy
(define* (call-with-vec size thunk #:optional (fill #f) (type 'f64))
  "Call THUNK with a new SIZE-d and TYPEd vector FILLed with data.
Free the vector afterwards."
  (let* ((vec (alloc size fill type)))
    (with-cleanup
     (free vec)
     (thunk vec))))

(define-syntax-rule (with (vec size arg ...) body ...)
  "Run BODY with VEC bound to SIZE-d vector FILLed with data."
  (call-with-vec
   size
   (lambda (vec)
     body ...)
   arg ...))

(define (for-vec thunk vec)
  "Call THUNK with (INDEX VALUE) of every element in VEC."
  (let ((len (length vec)))
    (do ((idx 0 (1+ idx)))
        ((= idx len))
      (thunk idx (get vec idx)))))
(define for-each for-vec)
