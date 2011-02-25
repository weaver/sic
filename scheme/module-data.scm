;;;; Environment (a dictionary). Used for env, export, and gensym.
(define make-env list)

(define env? list?)

(define (env-bind env binding val)
  (cons (cons binding val)
        env))

(define (env-lookup env binding)
  (and-let* ((box (memq env binding)))
    (cdr box)))

(define (env-set! env binding value)
  (and-let* ((box (memq env binding)))
    (set-car! box value)
    #t))

(define (unspecified) unspecified)

(define (unspecified? obj) (eq? unspecified obj))

;;;; Module
(define-record-type rtd/module
  (make-module* name env export gensym tail)
  module*?
  (name module-name*)
  (env module-env set-module-env!)
  (export module-export set-module-export!)
  (gensym module-gensym set-module-gensym!)
  (tail module-tail))

(define (module-cons name tail)
  (make-module* name
                (make-env)
                (make-env)
                (make-env)
                tail))

(define module-car identity)

(define module-cdr module-tail)

(define (module-null) module-null)

(define (module-null? obj)
  (eq? obj module-null))

(define (module? obj)
  (or (null-module? obj)
      (module*? obj)))

(define (module-foldr proc nil module)
  (foldr* proc nil module module-car module-cdr module-null?))

(define (module-map proc module)
  (module-foldr (lambda (x xs)
                  (cons (proc x) xs))
                '()
                module))

(define (module-name module)
  (module-map module-name* module))

(define (make-gensym module)
  (let ((seed (+ (gensym-lookup module 'seed) 1)))
    (gensym-bind! module 'seed seed)
    (gensym-bind! module seed unspecified)))
