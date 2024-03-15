(define-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (foreign-fn
            make-c-ptr
            sequence?
            sequence-ref
            sequence-length
            for-sequence))

(define libgslcblas (load-foreign-library "libgslcblas.so" #:global? #t))
;; (define libgslcblas (load-foreign-library "/home/aartaka/.guix-profile/lib/libgslcblas.so" #:global? #t))
(define libgsl (load-foreign-library "libgsl.so"))
;; (define libgsl (load-foreign-library "/home/aartaka/.guix-profile/lib/libgsl.so"))

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
      (let rec ((idx 0))
        (when (< idx ((if (list? seq)
                          length
                          vector-length)
                      seq))
          (thunk idx ((if (list? seq)
                          list-ref
                          vector-ref)
                      seq idx))
          (rec (1+ idx))))
      (error "for-seq called on a non-sequence: " seq)))

(define* (foreign-fn name args #:optional (return-type int))
  "Generate `foreign-library-function' for GSL from a shorter form."
  (foreign-library-function
   libgsl name
   #:return-type return-type
   #:arg-types args))

(define* (make-c-ptr type #:optional (val 0))
  (make-c-struct (list type) (list val)))

(define (strerror errno)
  (pointer->string ((foreign-fn "gsl_strerror" (list int) '*) errno)))

(define (set-error-handler handler)
  "Bind a new HANDLER as GSL error handler.
HANDLER must be an (#:optional reason file line errno #:rest etc)
procedure. A more relaxed arglist of (reason file line errno) is fine
according to GSL docs, but is unreliable in practice."
  ((foreign-fn "gsl_set_error_handler" '(*) '*)
   (cond
    ((procedure? handler)
     (procedure->pointer
      void
      handler
      `(* * ,int ,int)))
    ((pointer? handler)
     handler))))

;; (set-error-handler
;;  (lambda* (#:optional (reason "unknown reason") (file "unknown-file") (line -1) (errno -1) #:rest rest)
;;    (let ((error-text
;;           (format #f "Error ~d (~a at ~a:~d): ~a"
;;                   errno (strerror errno)
;;                   (if (pointer? file)
;;                       (pointer->string file)
;;                       file)
;;                   line
;;                   (if (pointer? reason)
;;                       (pointer->string reason)
;;                       reason))))
;;      (display error-text)
;;      (newline)
;;      (error error-text))))
