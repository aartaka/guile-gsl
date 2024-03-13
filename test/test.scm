;; Run with
;; guile --debug -L . test/test.scm
(define-module (lmdb tests)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign)
  #:use-module (gsl vectors)
  #:use-module (gsl matrices)
  #:use-module (gsl blas)
  #:use-module (gsl stat))

(test-begin "stat")
(define stat-test (make-c-struct (make-list 10 double) (iota 10)))
(test-equal 9/2 (inexact->exact (mean stat-test 10)))
(test-assert (< 9.1 (variance stat-test 10) 9.2))
(test-assert (< 3.0 (standard-deviation stat-test 10) 3.1))
(test-end "stat")
