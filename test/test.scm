;; Run with
;; guile --debug -L . test/test.scm
(define-module (lmdb tests)
  #:use-module (srfi srfi-64)
  #:use-module (system foreign)
  #:use-module (gsl vectors)
  #:use-module (gsl matrices)
  #:use-module (gsl blas)
  #:use-module (gsl stat))

(test-begin "vector-allocation")
;; Empty vector
(define vec0 (vec-alloc 0))
(test-eqv 0 (vec-length vec0))
;; Regular garbage-initialized
(define vec1 (vec-alloc 10))
(test-eqv 10 (vec-length vec1))
;; Initializer as number.
(define vec2 (vec-alloc 6 0))
(test-eqv 6 (vec-length vec2))
(test-eqv 0.0 (vec-get vec2 0))
(test-eqv 0.0 (vec-get vec2 3))
;; Fill
(vec-fill! vec2 8.0)
(test-eqv 8.0 (vec-get vec2 0))
(test-eqv 8.0 (vec-get vec2 5))
;; Initializer as vector.
(define vec3 (vec-alloc 3 #(1 -2 3)))
(test-eqv 3 (vec-length vec3))
(test-eqv 1.0 (vec-get vec3 0))
(test-eqv -2.0 (vec-get vec3 1))
(test-eqv 3.0 (vec-get vec3 2))
;; Vector copy
(define vec3-copy1 (vec-alloc 3))
(vec-copy! vec3 vec3-copy1)
(test-eqv 3.0 (vec-get vec3-copy1 2))
(define vec3-copy2 (vec-copy vec3))
(test-eqv 3.0 (vec-get vec3-copy2 2))
;; Vector callocation
(define vec-calloc-ed (vec-calloc 8))
(test-eqv 8 (vec-length vec-calloc-ed))
(test-eqv 0.0 (vec-get vec-calloc-ed 0))
(test-eqv 0.0 (vec-get vec-calloc-ed 7))
(test-end "vector-allocation")

(test-begin "vector-aggregation")
;; Predicates
(test-assert (vec-null? vec-calloc-ed))
(test-assert (vec-non-negative? vec1))
(test-assert (vec-positive? vec2))
(test-assert (vec-equal? vec3 vec3-copy1))
;; Aggregates
(test-eqv 48.0 (vec-sum vec2))
(test-eqv 3.0 (vec-max vec3))
(test-eqv 2 (vec-max-index vec3))
(test-eqv -2.0 (vec-min vec3))
(test-eqv 1 (vec-min-index vec3))
(test-end "vector-aggregation")

(test-begin "vector-ops")
(vec-add! vec3-copy1 vec3-copy2)
(test-assert (vec-equal? vec3-copy2 vec3))
(test-assert (not (vec-equal? vec3-copy1 vec3)))
(vec-scale! vec3-copy2 2)
(test-assert (vec-equal? vec3-copy1 vec3-copy2))
(vec-subtract! vec3-copy1 vec3)
(test-assert (vec-equal? vec3-copy1 vec3))
(vec-divide! vec3-copy1 vec3)
(test-eqv 1.0 (vec-get vec3-copy1 0))
(vec-add-constant! vec3-copy1 2)
(test-eqv 3.0 (vec-get vec3-copy1 0))
(test-end "vector-ops")

(test-begin "cleanup")
;; Free all vec3
;; Free all the vectors
(vec-free vec0)
(vec-free vec1)
(vec-free vec2)
(vec-free vec3-copy2)
(vec-free vec3-copy1)
(vec-free vec3)
(vec-free vec-calloc-ed)
(test-end "cleanup")

(test-begin "stat")
(define stat-test (make-c-struct (make-list 10 double) (iota 10)))
(test-equal 9/2 (inexact->exact (mean stat-test 10)))
(test-assert (< 9.1 (variance stat-test 10) 9.2))
(test-assert (< 3.0 (standard-deviation stat-test 10) 3.1))
(test-end "stat")
