(define (cps-application s1 application)
  (let ((proc (car application))
        (cont (ksym s1))
        (s2 (syntax-extend s1)))
    `(lambda (,cont)
       (,(cps-arguments s2 (cdr application))
        (lambda (args)                  ; new continuation
          ,(if (lambda? proc)
               `(,(cps-sequence s2 proc) ,cont)
               `(,cont (apply ,proc args))))))))

(define (cps-arguments s1 arguments)
  (let ((symbols (map (lambda (head)
                        (if (atom? head) head (vsym s1)))
                      arguments))
        (cont (ksym s1)))
    `(lambda (,cont)
       ,(foldl (lambda (sym head tail)
                 (if (atom? head)
                     tail
                     (let ((s2 (syntax-extend s1)))
                       `(,(cps-application s2 head)
                         (lambda (,sym)  ; new continuation
                           ,tail)))))
               `(,cont (list ,@symbols))
               symbols
               arguments))))

;; for testing, this makes the gensyms all start at 0
(define (tm) (make-syntax-closure "test" syntax-null))

(assert
 (cps-arguments (tm) '(1 2)) => '(lambda (test:k:1) (test:k:1 1 2))
 (cps-arguments (tm) `(foo (+ 1 2)))
 =>
 '(lambda (test:k:2)
    ((lambda (foo:test:k:1)
       ((lambda (foo:foo:test:k:1)
          (foo:foo:test:k:1 (list 1 2)))
        (lambda (args)
          (foo:test:k:1 (apply + args)))))
     (lambda (test:v:1)
       (test:k:2 (list bar test:v:1)))))
 )

(define (cps-begin module body)
  (let ((value (gensym module))
        (final (last body))
        (outer (gensym module)))
    `(lambda (,outer)
       ,(foldl (lambda (head tail)
                 (if (atom? head)
                     tail               ; an error?
                     `(,(cps-application module head)
                       (lambda (,value) ; new continuation
                         ,tail))))
               `(,outer ,(if (atom? final) final value))
               (cdr body)))))

(cps-begin (tm) '(begin (+ 4 5) t))


'(lambda (test:1)
   ((lambda (cont)
      ((lambda (cont)
         (cont (list 4 5)))
       (lambda (args)
         (cont (apply + args)))))
    (lambda (test:0) (test:1 t))))

(assert
 (cps-begin (tm) '(begin 1 2)) => '(lambda (cont) (cont 2))
 (cps-begin (tm) '(begin (+ 4 5) t)) =>
 '(lambda (cont)
    ((lambda (cont)
       ((lambda (cont)
          (cont (list 4 5)))
        (lambda (args)
          (cont (apply + args)))))
     (lambda (test:0)
       (cont t))))
 )
