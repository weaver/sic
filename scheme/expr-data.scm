;;;; An expr datatype that combines the module with the AST. This
;;;; thing will be used to a) make sure that we can report the source
;;;; with the module and b) to thread the world state through a
;;;; straightforward CPS mechanism. It should allow our sufficiently
;;;; complex code to look just like naive code.

(define (cons-expr ast tail)
  (cons (make-expr ast tail)
        tail))

(define (expr-ast expr)
  (car (car expr)))

(define (expr-module cdr)
  (cdr (car expr)))

(define (expr-cdr expr)
  (cdr expr))

(define (atom? expr)
  (not (pair? (expr-ast expr))))

(define (application? expr)
  (pair? (expr-ast expr)))

(define (expr-null? expr)
  (null? (expr-ast expr)))

(define (variable?* expr boundary-tag)
  (and-let* ((var (expr-ast expr))
             ((atom? var)))
    (let foldr ((mod (expr-module expr)))
      (if (or (module-null? mod)
              (eq? (module-tag mod) boundary-tag))
          #f
          (or (module-lookup mod var)
              (foldr (module-cdr mod)))))))

(define (primitive? expr)
  (variable?* expr 'module))

(define (variable? expr)
  (variable?* expr #f))

(define *k* '*k*)

(define (gensym expr tag)
  (make-gensym (expr-module expr)))
