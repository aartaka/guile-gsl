 (define-module (gsl utils)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs base)
  #:export-syntax (assert-types)
  #:export (make-c-ptr
            sequence?
            sequence-ref
            sequence-length
            for-sequence
            foreign-fn))

(define (sequence? seq)
  (or (list? seq)
      (vector? seq)))

(define (sequence-ref seq k)
  ((if (list? seq)
       list-ref
       vector-ref)
   seq k))

(define (sequence-length seq)
  "Return the length of SEQ, whether list of vector."
  ((if (list? seq)
       length
       vector-length)
   seq))

(define (for-sequence thunk seq)
  "Walk the elements of SEQ (list/vector) calling THUNK on index & value."
  (if (sequence? seq)
      (do ((len ((if (list? seq)
                     length
                     vector-length)
                 seq))
           (idx 0 (1+ idx)))
          ((= idx len))
        (thunk idx ((if (list? seq)
                        list-ref
                        vector-ref)
                    seq idx)))
      (error "for-seq called on a non-sequence: " seq)))

(define* (make-c-ptr type #:optional (val 0))
  (make-c-struct (list type) (list val)))

(define* (foreign-fn name args #:optional (return-type int))
  "Generate `foreign-library-function' for GSL from a shorter form."
  (foreign-library-function
   libgsl name
   #:return-type return-type
   #:arg-types args))

(define-syntax-rule (assert-types thing type ...)
  (assert
   (or (type thing) ...)))
