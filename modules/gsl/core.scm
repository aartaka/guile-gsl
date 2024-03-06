(define-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (foreign-fn
            make-c-ptr
            sequence?
            for-sequence))

(define libgsl (load-foreign-library "libgsl.so"))
;; (define libgsl (load-foreign-library "/home/aartaka/.guix-profile/lib/libgsl.so"))

(define (1+ x)
  (+ 1 x))

(define (sequence? seq)
  (or (list? seq)
      (vector? seq)))

(define (for-sequence thunk seq)
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
  "Generate `foreign-library-function' from a shorter form."
  (foreign-library-function
   libgsl name
   #:return-type return-type
   #:arg-types args))

(define* (make-c-ptr type #:optional (val 0))
  (make-c-struct (list type) (list val)))

(define (strerror errno)
  (pointer->string ((foreign-fn "gsl_strerror" (list int) '*) errno)))

(define (set-error-handler handler)
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
;;  (lambda* (#:optional reason file line (errno -1) #:rest rest)
;;    (let ((error-text
;;           (format #f "Error ~d (~a ~a:~d): ~a"
;;                   errno (strerror errno) (pointer->string file) line
;;                   (pointer->string reason))))
;;      (display error-text)
;;      (newline)
;;      (error error-text))))
