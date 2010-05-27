;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7

(define (atom? obj)
  (not (or (pair? obj)
           (null? obj))))

;;;; Environment
(define (env-lookup-box env sym)
  (call/cc
   (lambda (return)
     (map (lambda (frame)
            (map (lambda (binding)
                   (if (eq (car binding) sym)
                       (return (cdr binding))))
                 frame))
          env)))
  'lookup-error)

(define (env-lookup env sym)
  (car (env-lookup-box env sym)))

(define (env-set! env sym val)
  (let ((box (env-lookup-box env sym)))
    (set-car! box (list val))))

(define (env-define! env sym val)
  (set-car! env (cons (list sym val)
                      (car env))))

(define (env-bind env sym val)
  (cons (list (list sym val))
        env))

;;;; Analysis
(define (analyze-self-eval e)
  (lambda (env) e))

(define (analyze-quoted e)
  (let ((q (quote e)))
    (lambda (env) q)))

(define (analyze-variable e)
  (lambda (env) (env-lookup e)))

(define define-variable cadr)
(define define-value caddr)

(define (analyze-define e)
  (let ((var (define-variable e))
        (val (analyze (define-value e))))
    (lambda (env) (env-define! env var val))))

(define set-variable cadr)
(define set-value caddr)

(define (analyze-set! e)
  (let ((var (set-variable e))
        (val (analyze (set-value e))))
    (lambda (env) (env-set! env var val))))

(define if-predicate cadr)
(define if-then caddr)
(define if-else cadddr)

(define (analyze-if e)
  (let ((pred? (analyze (if-predicate e)))
        (then  (analyze (if-then e)))
        (else  (analyze (if-else e))))
    (lambda (env)
      (if (pred? env) (then env) (else env)))))

(define lambda-formal cadr)
(define lambda-body caddr)

(define (analyze-lambda e)
  (let ((vars (lambda-formal e))
        (body (analyze-begin (lambda-body e))))
    (lambda (env)
      (call-proc vars body env))))

(define (analyze-begin e)

  )

;;;; Old stuff
(define (compile-literal env form)
  (if (null? form)
      '()
      (let ((head (car form)))
        (if (atom? head)
            (inter-symbol env head (compile-literal env (cdr form)))
            form))))

(assert
 (compile-literal '() '())
  => '()
 (compile-literal '() '(foo))
  => '((foo))
 (compile-literal '() '(foo (bar (baz zup))))
  => '((foo (bar (baz zup)))))

(define (compile-define env form)
  )

(define (compile-lambda env form)

  )

(define (compile form env code)
  (if (null? form)
      (create-runnable env code)
      (let ((head (car form))
            (tail (cdr form)))
        (cond ((eq 'quote head)
               (compile-literal tail))
              ((eq 'define head)
               (compile-define env tail))
              ((eq 'define-syntax head)
               (compile-syntax env tail))
              ((eq 'lambda head)
               (compile-lambda tail))
              ((eq 'if head)
               (compile-if tail))
              (else (error "Syntax Error"))))))

(define source0
  '(begin
     (lambda (x) x)))
