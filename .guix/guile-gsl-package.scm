(define-module (guile-gsl-package)
 #:use-module (gnu packages guile)
 #:use-module (gnu packages maths)
 #:use-module (guix packages)
 #:use-module (guix gexp)
 #:use-module (guix utils)
 #:use-module (guix build-system guile)
 #:use-module (guix git-download)
 #:use-module ((guix licenses) #:prefix license:))

(define-public gsl-debug
  (package/inherit
      gsl
    (outputs '("out"
               "static"
               "debug"))))

(define-public guile-gsl-git
  (package
    (name "guile-gsl-git")
    (version "0.0.1")
    (source (local-file ".."
                        "guile-gsl-git-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (build-system guile-build-system)
    (arguments
     (list #:source-directory "modules"
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'substitute-gsl-so
                 (lambda _
                   (let ((gsl (string-append #$(this-package-input "gsl")
                                             "/lib/libgsl.so"))
                         (gslcblas (string-append #$(this-package-input "gsl")
                                                  "/lib/libgslcblas.so")))
                     (substitute* '("modules/gsl/core.scm")
                       (("libgsl.so")
                        gsl)
                       (("libgslcblas.so")
                        gslcblas))
                     #t))))))
    (native-inputs (list guile-3.0))
    (inputs (list guile-3.0 gsl))
    (home-page "https://github.com/aartaka/guile-gsl")
    (synopsis "Bindings for GNU Scientific library.")
    (description "Scheme wrapper around libgsl.so.")
    (license license:gpl3+)))

guile-gsl-git
