(define-module (gsl utils)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:export (make-c-ptr
            sequence?
            sequence-ref
            sequence-length
            for-sequence))

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
