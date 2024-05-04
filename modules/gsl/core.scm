(define-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (libgsl
            libgslcblas
            set-error-handler
            strerror))

(define libgslcblas (load-foreign-library "libgslcblas.so" #:global? #t))
;; (define libgslcblas (load-foreign-library "/home/aartaka/.guix-profile/lib/libgslcblas.so" #:global? #t))
(define libgsl (load-foreign-library "libgsl.so"))
;; (define libgsl (load-foreign-library "/home/aartaka/.guix-profile/lib/libgsl.so"))

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
