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
(define vec3-copy2 (vec-copy! vec3))
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
;; vec3-copy1 is #(2.0 -4.0 6.0)
(vec-scale! vec3-copy2 2)
;; vec3-copy2 is doubled too
(test-assert (vec-equal? vec3-copy1 vec3-copy2))
(vec-subtract! vec3-copy1 vec3)
;; vec3-copy1 is #(1.0 -2.0 3.0) back again
(test-assert (vec-equal? vec3-copy1 vec3))
(vec-divide! vec3-copy1 vec3)
;; vec3-copy1 is #(1.0 1.0 1.0)
(test-eqv 1.0 (vec-get vec3-copy1 0))
(vec-add-constant! vec3-copy1 2)
;; vec3-copy1 is #(3.0 3.0 3.0)
(test-eqv 3.0 (vec-get vec3-copy1 0))
(test-end "vector-ops")

(test-begin "matrix-allocation")
;; Empty matrix
(define mtx0 (mtx-alloc 0 0 0))
(test-assert mtx0)
(test-equal 0 (mtx-rows mtx0) (mtx-columns mtx0))
(define mtx1 (mtx-alloc 3 3 1.0))
(test-equal 3 (mtx-rows mtx1) (mtx-columns mtx1))
(test-eqv 1.0 (mtx-get mtx1 0 0))
;; Sequence-filled matrix
(define mtx2 (mtx-alloc 3 3 #(#(1 2 3)
                              (4 0 1)
                              #(1 0 2))))
(test-equal 3 (mtx-rows mtx2) (mtx-columns mtx2))
(test-eqv 4.0 (mtx-get mtx2 1 0))
;; Copying
(define mtx2-copy1 (mtx-alloc 3 3))
(mtx-copy! mtx2 mtx2-copy1)
(test-eqv 1.0 (mtx-get mtx2-copy1 0 0))
(define mtx2-copy2 (mtx-copy! mtx2))
(test-eqv 1.0 (mtx-get mtx2-copy2 0 0))
;; Calloc-ed
(define mtx-calloc-ed (mtx-calloc 2 2))
(test-equal 2 (mtx-rows mtx-calloc-ed) (mtx-columns mtx-calloc-ed))
(test-eqv 0.0 (mtx-get mtx-calloc-ed 0 1))
(test-end "matrix-allocation")

(test-begin "matrix-aggregation")
;; Predicates
(test-assert (mtx-null? mtx-calloc-ed))
(test-assert (mtx-positive? mtx1))
(test-assert (not (mtx-positive? mtx2)))
(test-assert (mtx-non-negative? mtx2))
(test-assert (mtx-equal? mtx2 mtx2-copy1))
(test-assert (mtx-equal? mtx2 mtx2-copy2))
;; Aggregation
(test-eqv 4.0 (mtx-max mtx2))
(test-eqv 0.0 (mtx-min mtx2))
(test-equal '(1 0) (mtx-max-index mtx2))
(test-equal '(1 2) (mtx-min-index mtx2))
(test-eqv 3.0 (mtx-norm1 mtx1))
(test-end "matrix-aggregation")

(test-begin "cleanup")
;; Free all the matrices
(mtx-free mtx0)
(mtx-free mtx1)
(mtx-free mtx2)
(mtx-free mtx2-copy1)
(mtx-free mtx2-copy2)
(mtx-free mtx-calloc-ed)
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
