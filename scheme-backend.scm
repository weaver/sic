;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7

;;;; Utility
(define-syntax assert
  (syntax-rules (=>)
    ((_ expr => val)
     (if (not (equal? expr val))
         (error "assertion failed" 'expr "=>" 'val)
         #t))
    ((_ e1 => v1 e2 ...)
     (and (assert e1 => v1) (assert e2 ...)))))

(define-syntax if-let1
  (syntax-rules ()
    ((_ ((sym exp)) then else)
     (let ((sym exp))
       (if sym then else)))
    ((_ ((exp)) then else)
     (if exp then else))))

(define-syntax if-let*
  (syntax-rules ()
    ((_ (b1 b2 ...) then else)
     (if-let1 (b1)
              (if-let* (b2 ...) then else)
              else))
    ((_ (b1) then else)
     (if-let1 (b1) then else))
    ((_ () then else)
     (begin then))
    ;; provide a default else clause
    ((_ bindings then)
     (if-let* bindings then #f))))

(define-syntax and-let*
  (syntax-rules ()
    ((_ bindings body ...)
     (if-let* bindings (begin body ...)))))

(assert
 (if-let1 ((#f)) 1 2)             => 2
 (if-let1 ((foo 1)) foo 2)        => 1
 (if-let* ((#f)) 1 2)             => 2
 (if-let* ((foo 1)) foo 2)        => 1
 (if-let* ((foo 1) (bar 1)) (+ foo bar) #f)          => 2
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar) #f)  => #f
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar))     => #f
 (and-let* ((foo 1) (bar 1)) (+ foo bar))            => 2
 (and-let* ((foo 1) (bar 1)) (+ foo bar) 'foo)       => 'foo
 (and-let* ((foo 1) (bar (= foo 2))) (+ foo bar))    => #f
 )

(define (atom? obj)
  (not (or (pair? obj)
           (null? obj))))

;;;; Environment
(define (e-undefined sym) (error "undefined symbol" sym))
(define (env-get-box env sym)
  (call/cc
   (lambda (return)
     (map (lambda (frame)
            (map (lambda (binding)
                   (if (eq (car binding) sym)
                       (return (cdr binding))))
                 frame))
          env)))
  (e-undefined sym))

(define (env-get env sym)
  (car (env-get-box env sym)))

(define (env-set! env sym val)
  (set-car! (env-get-box env sym) val))

(define (env-define! env sym val)
  (if-let* ((box (env-get-box env sym)))
           (set-car! box val)
           (set-car! env (cons (list sym val)
                               (car env)))))

(define (env-extend env sym val)
  (cons (list (list sym val))
        env))

;;;; Data types
(define-record-type rtd/procedure
  (make-procedure env body args)
  procedure?
  (env procedure-env)
  (body procedure-body)
  (args procedure-args))

;;;; Analysis
(define (analyze-self-eval e)
  (lambda (env) e))

(define (analyze-quoted e)
  (let ((q (quote e)))
    (lambda (env) q)))

(define (analyze-variable e)
  (lambda (env) (env-get e)))

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
      (make-procedure env body vars))))

(define (e-empty-begin) (error "empty begin"))
(define (analyze-begin e)
  (define (seq one two)
    (lambda (env) (one env) (two env)))
  (if (null? e) (e-empty-begin)
      (let lp ((head (car e)) (tail (cdr e)))
        (if (null? tail)
            head
            (lp (seq head (car tail))
                (cdr tail))))))

(define application-op car)
(define application-args cdr)
(define (analyze-application e)
  (let ((proc (analyze (application-op e)))
        (args (map analyze (application-args e))))
    (lambda (env)
      (execute-application proc (map (lambda (proc)
                                       (proc env))
                                     args)))))

(define (execute-application proc args)
  (cond ((primitive? proc)
         (proc args))
        ((procedure? proc)
         (
          (env-bind))
         ))
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
