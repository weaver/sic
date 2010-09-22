(define-record-type rtd/syntax-closure
  (make-syntax-closure* name gensym symbol cdr)
  (name syntax-name)
  (gensym syntax-gensym set-syntax-gensym!)
  (symbol syntax-table)
  (cdr syntax-cdr))

(define (make-syntax-closure name tail)
  (make-syntax-closure* name 0 (make-symbol-table) tail))

(define (syntax-null) syntax-null)
(define (syntax-null? obj) (eq? obj syntax-null))

(define (syntax-ref s key)
  (table-ref (syntax-table s) key))

(define (syntax-set! s key val)
  (table-set! (syntax-table s) key val))

(define (syntax++ syntax)
  (let ((inc (+ (syntax-gensym syntax) 1)))
   (set-syntax-gensym! syntax inc)
   inc))

(define (syntax-extend s1)
  (make-syntax-closure "foo" s1))

(define (syntax-foldr proc nil syntax)
  (foldr* proc nil syntax identity syntax-cdr syntax-null?))

(define (syntax-map proc s)
  (syntax-foldr (lambda (s a) (cons (proc s) a)) '() s))

(define (syntax-for-each proc s)
  (syntax-foldr (lambda (s a) (proc s)) '() s))

(define (syntax-id s)
  (syntax-map syntax-name s))

(define (syntax-id-string s)
  (string-join ":" (syntax-map syntax-name s)))

(define (gensym* prefix syntax)
  (string->symbol
   (string-append
    (syntax-id-string syntax)
    ":"
    prefix
    ":"
    (number->string (syntax++ syntax)))))

(define (vsym s) (gensym* "v" s))
(define (ksym s) (gensym* "k" s))

(define (variable? s e)
  (call/cc
   (lambda (k)
     (syntax-for-each
      (lambda (s)
        (let ((val (syntax-ref s e)))
          (k val)))
      s)
     #f)))

(define (constant? e)
  (or (number? e) (character? e) (string? e)))

(define (primitive? e)
  (procedure? e))

(define (atom? e)
  (not (pair? e)))

(define (lambda? e)
  #f)

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
