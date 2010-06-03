;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7

;;;; Utility
;; you need srfi-9 from somewhere or another. This is for PLT
(require (lib "9.ss" "srfi"))

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
    ((_ ((sym expr)) then else)
     (let ((sym expr))
       (if sym then else)))
    ((_ ((expr)) then else)
     (if expr then else))))

(define-syntax if-let*
  (syntax-rules ()
    ;; provide a default else clause
    ((_ bindings then) (if-let* bindings then #f))
    ((_ () then else) (begin then))
    ((_ (b1) then else) (if-let1 (b1) then else))
    ((_ (b1 b2 ...) then else)
     (if-let1 (b1)
              (if-let* (b2 ...) then else)
              else))))

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

;;;; Environment
(define (env-get-box env sym)
  (call/cc
   (lambda (return)
     (map (lambda (frame)
            (map (lambda (binding)
                   (and (eq? (car binding) sym)
                        (return (cdr binding))))
                 frame))
          env)))
  (error "undefined variable" sym))

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


;;;; Types and expression predicates
(define-record-type rtd/procedure
  (make-proc env body args)
  proc?
  (env proc-env)
  (body proc-body)
  (args proc-args))

(define (tagged-list? e tag)
  (and (pair? e) (eq? (car e) tag)))

(define (self-eval? e)
  (or (number? e)
      (string? e)))

(define (quoted? e) (tagged-list? e 'quote))
(define quote-body cdr)

(define (variable? e) (symbol? e))

(define (define? e) (tagged-list? e 'define))
(define define-symbol cadr)
(define define-value caddr)

(define (set? e) (tagged-list? e 'set!))
(define set-symbol cadr)
(define set-value caddr)

(define (if? e) (tagged-list? e 'if))
(define if-predicate cadr)
(define if-then caddr)
(define if-else cadddr)

(define (lambda? e) (tagged-list? e 'lambda))
(define lambda-formal cadr)
(define lambda-body cddr)

(define (begin? e) (tagged-list? e 'begin))
(define begin-body cdr)

(define (appliation? e) (pair? e))
(define application-op car)
(define application-args cdr)

;;;; Analysis
(define (analyze-self-eval e)
  (lambda (env) e))

(define (analyze-quoted e)
  (let ((q (quote (quote-body e))))
    (lambda (env) q)))

(define (analyze-variable e)
  (lambda (env) (env-get e)))

(define (analyze-define e)
  (let ((var (define-symbol e))
        (val (analyze (define-value e))))
    (lambda (env) (env-define! env var val))))

(define (analyze-set! e)
  (let ((var (set-symbol e))
        (val (analyze (set-value e))))
    (lambda (env) (env-set! env var val))))

(define (analyze-if e)
  (let ((pred? (analyze (if-predicate e)))
        (then  (analyze (if-then e)))
        (else  (analyze (if-else e))))
    (lambda (env)
      (if (pred? env) (then env) (else env)))))

(define (analyze-lambda e)
  (let ((vars (lambda-formal e))
        (body (analyze-begin (lambda-body e))))
    (lambda (env)
      (make-proc env body vars))))

(define (analyze-begin e)
  (define (seq one two)
    (lambda (env) (one env) (two env)))
  (if (null? e)
      (error "empty begin")
      (let lp ((head (car e)) (tail (cdr e)))
        (if (null? tail)
            head
            (lp (seq head (car tail))
                (cdr tail))))))

(define (analyze-application e)
  (let ((proc (analyze (application-op e)))
        (args (map analyze (application-args e))))
    (lambda (env)
      (execute-application proc (map (lambda (proc)
                                       (proc env))
                                     args)))))

(define (execute-application proc args)
  (cond ((procedure? proc)
         (proc args))
        ((proc? proc)
         ((proc-body proc)
          (env-extend (proc-vars proc)
                      args
                      (proc-env proc))))
        (else
         (error "invalid procedure call" proc))))

(define (analyze expr)
  (cond ((self-eval? expr) (analyze-self-eval expr))
        ((quoted? expr) (analyze-quoted expr))
        ((variable? expr) (analyze-variable expr))
        ((set? expr) (analyze-set! expr))
        ((define? expr) (analyze-define expr))
        ((if? expr) (analyze-if expr))
        ((lambda? expr) (analyze-lambda expr))
        ((begin? expr) (analyze-begin expr))
        ((application? expr) (analyze-application expr))
        (else
         (error "invalid expresssion" expr))))
