(define-record-type rtd/module
  (make-module* name symbol import export gensym)
  module?
  (name module-name)
  (symbol module-symbol)
  (import module-import set-module-import!)
  (export module-export set-module-export!)
  (gensym module-gensym set-module-gensym!))

(define (make-module name)
  (make-module* name #f #f 0))

(define (gensym module)
  (let ((gensym (module-gensym module))
        (prefix (string-append (module-name module) ":")))
    (set-module-gensym! module (+ gensym 1))
    (string->symbol
     (string-append prefix (number->string gensym)))))

(define (variable? module e)

  )

(define (constant? e)
  (or (number? e) (character? e) (string? e)))

(define (atom e)
  (or (constant? e) (symbol? e)))

(define (cps-application module application)
  (let ((proc (car application)))
    `(lambda (cont)
       (,(cps-arguments module (cdr application))
        (lambda (args)                  ; new continuation
          (cont (apply ,proc args)))))))

(define (cps-arguments module arguments)
 (let ((symbols (map (lambda (head)
                        (if (atom? head) head (gensym module)))
                      arguments)))
    `(lambda (cont)
       ,(foldl (lambda (sym head tail)
                 (if (atom? head)
                     tail
                     `(,(cps-application module head)
                       (lambda (,sym)   ; new continuation
                         ,tail))))
               `(cont (list ,@symbols))
               symbols
               arguments))))

;; for testing, this makes the gensyms all start at 0
(define (tm) (make-module "test"))

(assert
 (cps-arguments (tm) '(1 2)) => '(lambda (cont) (cont 1 2))
 (cps-arguments (tm) `(foo (+ 1 2)))
 =>
 '(lambda (cont)
    ((lambda (cont)
       ((lambda (cont)
          (cont (list 1 2)))
        (lambda (args)
          (cont (apply + args)))))
     (lambda (test:0) (cont (list foo test:0)))))
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
