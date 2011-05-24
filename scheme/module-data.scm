(define-record-type rtd/module
  (make-module* name env export gensym tail)
  module*?
  (name module-name*)
  (tag module-tag)
  (env module-env set-module-env!)
  (export module-export set-module-export!)
  (gensym module-gensym set-module-gensym!)
  (tail module-tail))

(define (module-cons name tag tail)
  (make-module* name
                tag
                '()
                '()
                '()
                tail))

(define module-car identity)
(define module-cdr module-tail)

(define (module-null) module-null)
(define (module-null? obj) (eq? obj module-null))

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

(define (unspecified) unspecified)
(define (unspecified? obj) (eq? unspecified obj))

(define box? pair?)
(define unbox cdr)

(define (module-lookup* module slot sym)
  (and-let* ((env (slot module))
             (box (memq env binding)))
    box))

(define (module-bind*! module slot set-slot! sym val)
  (let* ((env (slot module))
         (box (memq env sym)))
    (if box
        (set-car! box value)
        (set-slot! module (cons (cons binding value) env)))))

(define (module-lookup module sym)
  (module-lookup* module module-env sym))

(define (module-bind! module sym val)
  (module-bind*! module module-env set-module-env! sym val))

(define (module-export module sym)
  (module-lookup* module module-export sym))

(define (module-export! module sym val)
  (module-bind*! module module-export set-module-export! sym val))

(define (module-gensym module sym)
  (module-lookup* module module-gensym sym))

(define (module-gensym! module sym val)
  (module-bind*! module module-gensym set-module-gensym! sym val))
