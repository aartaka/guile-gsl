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
