(define-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (foreign-fn))

(define libgsl (load-foreign-library "libgsl.so"))
;; (define libgsl (load-foreign-library "/home/aartaka/.guix-profile/lib/libgsl.so"))

(define* (foreign-fn name args #:optional (return-type int))
  "Generate `foreign-library-function' from a shorter form."
  (foreign-library-function
   libgsl name
   #:return-type return-type
   #:arg-types args))

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
;;  (lambda* (reason #:optional file line errno)
;;    (let ((error-text (format #f "Error ~d (~a:~d): ~a"
;;                              errno (pointer->string file) line (pointer->string reason))))
;;      (display error-text)
;;      (newline)
;;      (error error-text))))
