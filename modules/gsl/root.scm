(define-module (gsl root)
  #:use-module (gsl utils)
  #:use-module (gsl core)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export-syntax (with)
  #:export ( ;; Solver types
            +bisection-solver+
            +false-position-solver+
            +brent-solver+
            ;; Polisher types
            +newton-polisher+
            +secant-polisher+
            +steffenson-polisher+
            ;; Solver API
            solver?
            wrap-solver
            unwrap-solver
            ;; Polisher API
            polisher?
            wrap-polisher
            unwrap-polisher
            ;; Functions on solvers/polishers
            name
            root
            upper-bound
            lower-bound
            set!
            alloc
            free
            iterate!
            ;; Stopping conditions
            test-interval
            test-delta
            test-residual
            ;; Utilities
            call-with
            optimize))

(define (deref ptr)
  (car (parse-c-struct ptr '(*))))

(define +bisection-solver+
  (deref (foreign-library-pointer libgsl "gsl_root_fsolver_bisection")))
(define +false-position-solver+
  (deref (foreign-library-pointer libgsl "gsl_root_fsolver_falsepos")))
(define +brent-solver+
  (deref (foreign-library-pointer libgsl "gsl_root_fsolver_brent")))

(define +newton-polisher+
  (deref (foreign-library-pointer libgsl "gsl_root_fdfsolver_newton")))
(define +secant-polisher+
  (deref (foreign-library-pointer libgsl "gsl_root_fdfsolver_secant")))
(define +steffenson-polisher+
  (deref (foreign-library-pointer libgsl "gsl_root_fdfsolver_secant")))

(define-wrapped-pointer-type <solver>
  solver?
  wrap-solver unwrap-solver
  (lambda (solver p)
    (format p "#<gsl ~a solver ~x state=~s|~s|~s>"
            (name solver)
            (pointer-address (unwrap-solver solver))
            (lower-bound solver)
            (root solver)
            (upper-bound solver))))
(define-wrapped-pointer-type <polisher>
  polisher?
  wrap-polisher unwrap-polisher
  (lambda (polisher p)
    (format p "#<gsl ~a polisher ~x root=~s>"
            (name polisher)
            (pointer-address (unwrap-polisher polisher))
            (root polisher))))

(define (name solver/polisher)
  "Name (e.g. \"bisection\", \"newton\") for the SOLVER/POLISHER method."
  (assert-types solver/polisher solver? polisher?)
  (pointer->string
   (if (solver? solver/polisher)
       ((foreign-fn "gsl_root_fsolver_name" '(*) '*)
        (unwrap-solver solver))
       ((foreign-fn "gsl_root_fdfsolver_name" '(*) '*)
        (unwrap-polisher polisher)))))

(define (root solver/polisher)
  "Current approximation of root in SOLVER/POLISHER."
  (assert-types solver/polisher solver? polisher?)
  (if (solver? solver/polisher)
      ((foreign-fn "gsl_root_fsolver_root" '(*) double)
       (unwrap-solver solver/polisher))
      ((foreign-fn "gsl_root_fdfsolver_root" '(*) double)
       (unwrap-polisher solver/polisher))))

(define (upper-bound solver)
  (assert-types solver solver?)
  ((foreign-fn "gsl_root_fsolver_x_upper" '(*) double)
   (unwrap-solver solver)))
(define (lower-bound solver)
  (assert-types solver solver?)
  ((foreign-fn "gsl_root_fsolver_x_lower" '(*) double)
   (unwrap-solver solver)))

(define (false? x)
  (not x))

(define (wrap-solver-function function)
  (assert-types function pointer? procedure?)
  (cond
   ((procedure? function)
    (make-c-struct '(* *)
                   (list (procedure->pointer double (lambda (x params)
                                                      (function x))
                                             (list double '*))
                         %null-pointer)))
   ((pointer? function)
    function)))

(define (wrap-polisher-function function derivative function+derivative)
  (assert-types function procedure? pointer?)
  (assert-types derivative procedure? pointer?)
  (assert-types function+derivative procedure? pointer?)
  (make-c-struct
   '(* * * *)
   (list (if (procedure? function)
             (procedure->pointer double (lambda (x params)
                                          (function x))
                                 `(,double *))
             function)
         (if (procedure? derivative)
             (procedure->pointer double (lambda (x params)
                                          (derivative x))
                                 `(,double *))
             function)
         (if (procedure? function+derivative)
             (procedure->pointer void (lambda (x params f df)
                                        (match (function+derivative x)
                                          ((fun deriv)
                                           (bytevector-ieee-double-native-set!
                                            (pointer->bytevector f (sizeof double))
                                            0 fun)
                                           (bytevector-ieee-double-native-set!
                                            (pointer->bytevector df (sizeof double))
                                            0 deriv))))
                                 `(,double * * *))
             function+derivative)
         %null-pointer)))


(define* (modify! solver/polisher
                  #:key (function #f) (derivative #f)
                  (function+derivative #f)
                  approximate-root upper lower)
  (if (solver? solver/polisher)
      (cond
       ((and function upper lower)
        ((foreign-fn "gsl_root_fsolver_set" `(* * ,double ,double) int)
         (unwrap-solver solver/polisher)
         (wrap-solver-function function) lower upper))
       ((or function upper lower)
        (warn "Expecting FUNCTION, a LOWER bound, and an UPPER bound.")))
      (cond
       ((and function derivative approximate-root)
        ((foreign-fn "gsl_root_fdfsolver_set" `(* * ,double) int)
         (unwrap-polisher solver/polisher)
         (wrap-polisher-function
          function derivative
          (or function+derivative
              (lambda (x) (list (function x)
                                (derivative x)))))
         approximate-root))
       ((or function derivative function+derivative approximate-root)
        (warn "Expecting at least FUNCTION, DERIVATIVE, and APPROXIMATE-ROOT.")))))
(define set! modify!)
(define reset! modify!)

(define* (alloc solver/polisher
                #:key (function #f) (derivative #f)
                (function+derivative #f)
                approximate-root upper lower)
  "Create a new SOLVER/POLISHER and initialize it.
For solvers, provide FUNCTION, UPPER, and LOWER.
For polishers, provide FUNCTION, DERIVATIVE, APPROXIMATE-ROOT, and,
optionally, FUNCTION+DERIVATIVE.
FUNCTION, DERIVATIVE, and FUNCTION+DERIVATIVE can be procedures or
pointers to foreign functions."
  (assert-types solver/polisher pointer?)
  (assert-types function pointer? procedure?)
  (cond
   ((member solver/polisher
            (list +bisection-solver+ +false-position-solver+ +brent-solver+))
    (let ((solver (wrap-solver
                   ((foreign-fn "gsl_root_fsolver_alloc" '(*) '*)
                    solver/polisher))))
      (modify! solver
               #:function function #:lower lower #:upper upper)
      solver))
   ((member solver/polisher
            (list +newton-polisher+ +secant-polisher+ +steffenson-polisher+))
    (let ((polisher (wrap-polisher
                     ((foreign-fn "gsl_root_fdfsolver_alloc" '(*) '*)
                      solver/polisher))))
      (modify! polisher
               #:function function
               #:derivative derivative
               #:function+derivative function+derivative
               #:approximate-root approximate-root)
      polisher))
   (else
    (error 'alloc (format #f "Cannot create a solver for type ~s" solver/polisher)))))
(define make alloc)

(define (free solver/polisher)
  (assert-types solver/polisher solver? polisher?)
  (cond
   ((solver? solver/polisher)
    ((foreign-fn "gsl_root_fsolver_free" '(*) void)
     (unwrap-solver solver/polisher)))
   ((polisher? solver/polisher)
    ((foreign-fn "gsl_root_fdfsolver_free" '(*) void)
     (unwrap-polisher solver/polisher)))))

(define (iterate! solver/polisher)
  "Iterate on SOLVER/POLISHER once.
Return current root approximation if successful, #f on error."
  (assert-types solver/polisher solver? polisher?)
  (let ((code (if (solver? solver/polisher)
                  ((foreign-fn "gsl_root_fsolver_iterate" '(*) int)
                   (unwrap-solver solver/polisher))
                  ((foreign-fn "gsl_root_fdfsolver_iterate" '(*) int)
                   (unwrap-polisher solver/polisher)))))
    (cond
     ((zero? code)
      (root solver/polisher))
     (else
      #f))))

(define (test-interval solver/polisher absolute-error relative-error)
  "Test that SOLVER/POLISHER upper/lower bounds are converging with *-ERRORs."
  (zero? ((foreign-fn "gsl_root_test_interval" (list double double double double) int)
          (lower-bound solver/polisher) (upper-bound solver/polisher)
          absolute-error relative-error)))
(define (test-delta x0 x1 absolute-error relative-error)
  "Test that X0 and X1 are converging with *-ERRORs."
  (zero? ((foreign-fn "gsl_root_test_delta" (list double double double double) int)
          x0 x1 absolute-error relative-error)))
(define (test-residual f absolute-error)
  "Test the residual value F against the ABSOLUTE-ERROR bound."
  (zero? ((foreign-fn "gsl_root_test_residual" (list double double) int)
          f absolute-error)))

(define (call-with solver/polisher thunk . args)
  "Call THUNK with newly `alloc'-ated SOLVER/POLISHER.
Free it automatically and return result of THUNK."
  (let* ((solver/polisher (apply alloc solver/polisher args))
         (result (thunk solver/polisher)))
    (free solver/polisher)
    result))

(define* (optimize solver/polisher iterations relative-error . args)
  "Return the root (or #f) for SOLVER/POLISHER.
Find it in under ITERATIONS, guided by RELATIVE-ERROR.
ARGS are as per `alloc' for SOLVER/POLISHER.

Example from GSL docs might be rewritten as:
(let ((a 1)
      (b 0)
      (c -5))
  (optimize
   +newton-polisher+ 100 0.001
   #:function (lambda (x)
                (+ (* a x x)
                   (* b x)
                   c))
   #:derivative (lambda (x)
                  (+ (* 2 a x)
                     b))
   #:approximate-root 5))"
  (apply
   call-with
   solver/polisher
   (lambda (solver/polisher)
     (do ((i 0 (1+ i))
          (approximation (iterate! solver/polisher)
                         (iterate! solver/polisher))
          (prev-approximation #f approximation))
         ((or (= i iterations)
              (cond
               ((solver? solver/polisher)
                (test-interval solver/polisher 0 relative-error))
               (prev-approximation
                (test-delta prev-approximation approximation 0 relative-error))
               (else
                #f))
              (eq? #f approximation))
          approximation)))
   args))

(define-syntax-rule (with (var solver/polisher args ...) body ...)
  "Run the BODY with VAR bound to newly allocated SOLVER/POLISHER (with ARGS)."
  (call-with
   solver/polisher
   (lambda (var)
     body ...)
   args ...))
