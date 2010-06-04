;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7
;;;; PLT r5rs compatibilty: (namespace-require 'r5rs)

;;;; Utility
(define-syntax assert
  (syntax-rules (=>)
    ;; arrow is an infix operator for equal?
    ((_ expr => val)
     (let ((result expr))
       (if (not (equal? result val))
           (error "assertion failed" 'expr '=> result 'not val)
           #t)))
    ;; optional arrow and value
    ((_ expr)
     (if (not expr)
         (error "assertion failed" 'expr)
         #t))
    ;; recursion with and without the arrow
    ((_ e1 => v1 e2 ...) (and (assert e1 => v1) (assert e2 ...)))
    ((_ e1 e2 ...) (and (assert e1) (assert e2 ...)))))

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

;; (define (foldr proc nil lst)
;;   (if (null? lst)
;;       nil
;;       (proc (car lst)
;;             (foldr proc nil (cdr lst)))))

;; (define (foldl proc nil lst)
;;   (if (null? lst)
;;       nil
;;       (foldl proc
;;              (proc (car lst) nil)
;;              (cdr lst))))

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
 ;; (foldr cons '() '(1 2 3))                           => '(1 2 3)
 ;; (foldl cons '() '(1 2 3))                           => '(3 2 1)
 )

;;;; Environment
(define (box val) (vector val))
(define (unbox box) (vector-ref box 0))
(define (set-box! box val) (vector-set! box 0 val))

(define (env-get-box env sym . undefined)
  (call/cc
   (lambda (return)
     (for-each (lambda (frame)
                 (for-each (lambda (binding)
                             (and (eq? sym (car binding))
                                  (return (cdr binding))))
                           frame))
               env)
     (if (null? undefined)
         (error "undefined variable" sym)
         (car undefined)))))

(define (env-get env sym)
  (unbox (env-get-box env sym)))

(define (env-set! env sym val)
  (set-box! (env-get-box env sym) val))

(define (env-define! env sym val)
  (if-let* ((cur (env-get-box env sym #f)))
           (set-box! cur val)
           (set-car! env (cons (cons sym
                                     (box val))
                               (car env)))))

(define (env-extend env bindings values)
  (cons (map (lambda (sym val)
               (cons sym (box val)))
             bindings
             values)
        env))

(let ()
  (define env `(((a . ,(box 1)) (b . ,(box 2)))))
  (assert
   (env-get env 'a)                    => 1
   (env-get env 'b)                    => 2
   (env-define! env 'c 3)
   (env-get env 'c)                    => 3
   (env-set! env 'a 12)
   (env-get env 'a)                    => 12
   (set! env (env-extend env '(e f g) '(7 8 9)))
   (env-get env 'g)                   => 9
   (env-get env 'a)                   => 12
   ))

;;;; Types and expression predicates

;;; eliminate srfi-9, just so we can use this with any scheme for now.
;;; this is the only runtime data type we're using so far.

;; (define-record-type rtd/proc
;;   (make-proc env body args)
;;   proc?
;;   (env proc-env)
;;   (body proc-body)
;;   (args proc-args))

(define (make-proc env body args)
  (vector make-proc env body args))
(define (proc? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) make-proc)))
(define (proc-env obj) (vector-ref obj 1))
(define (proc-body obj) (vector-ref obj 2))
(define (proc-args obj) (vector-ref obj 3))

;;; expression types
(define (tagged-list? e tag)
  (and (pair? e) (eq? (car e) tag)))

(define (self-eval? e)
  (or (boolean? e)
      (number? e)
      (string? e)))

(define (quoted? e) (tagged-list? e 'quote))
(define quote-body cadr)

(define (variable? e) (symbol? e))

;;; only (define sym val) for now. I'd prefer to fix this with a
;;; syntax expansion.
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
(define lambda-formals cadr)
(define lambda-body cddr)

(define (begin? e) (tagged-list? e 'begin))
(define begin-body cdr)

(define application-proc car)
(define application-args cdr)

(assert
 (and (self-eval? '2) (self-eval? "c") (self-eval? #t))
 (quoted? '(quote f))
 (quote-body '(quote f))             => 'f
 (define? '(define a '(b)))
 (define-symbol '(define a (b)))     => 'a
 (define-value '(define a (b)))      => '(b)
 (lambda? '(lambda (x) x))
 (lambda-formals '(lambda (x) x))    => '(x)
 (lambda-body '(lambda (x) x))       => '(x)
 (lambda-body '(lambda () 1))        => '(1)
 (if? '(if a b c))
 (if-predicate '(if a b c))          => 'a
 (if-then '(if a b c))               => 'b
 (if-else '(if a b c))               => 'c
 (application-proc '(+ 1 2))         => '+
 (application-args '(+ 1 2))         => '(1 2)
 )

;;;; Analysis
(define (analyze-self-eval e)
  (lambda (env) e))

(define (analyze-quoted e)
  (let ((q (quote-body e)))
    (lambda (env) q)))

(define (analyze-variable e)
  (lambda (env) (env-get env e)))

(define (analyze-define e)
  (let ((var (define-symbol e))
        (val (analyze (define-value e))))
    (lambda (env) (env-define! env var (val env)))))

(define (analyze-set! e)
  (let ((var (set-symbol e))
        (val (analyze (set-value e))))
    (lambda (env) (env-set! env var (val env)))))

(define (analyze-if e)
  (let ((pred? (analyze (if-predicate e)))
        (then  (analyze (if-then e)))
        (else  (analyze (if-else e))))
    (lambda (env)
      (if (pred? env) (then env) (else env)))))

(define (analyze-lambda e)
  (let ((syms (lambda-formals e))
        (body (analyze-begun (lambda-body e))))
    (lambda (env)
      (make-proc env body syms))))

(define (analyze-begin e)
  (analyze-begun (begin-body e)))

(define (analyze-begun exprs)
  (define (seq e1 e2)
    (lambda (env) (e1 env) (e2 env)))
  (define (lp head tail)
    (if (null? tail)
        head
        (lp (seq head (car tail))
            (cdr tail))))
  (if (null? exprs)
      (error "empty body")
      (lp (analyze (car exprs))
          (map analyze (cdr exprs)))))

(define (analyze-application e)
  (let ((proc (analyze (application-proc e)))
        (args (map analyze (application-args e))))
    (lambda (env)
      (execute-application
       (proc env)
       (map (lambda (arg)
              (arg env))
            args)))))

(define (execute-application proc args)
  (cond ((procedure? proc)
         (apply proc args))
        ((proc? proc)
         ((proc-body proc)
          (env-extend (proc-env proc)
                      (proc-args proc)
                      args)))
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
        ;; primitives
        ((procedure? expr) (analyze-self-eval expr))
        ((pair? expr) (analyze-application expr))
        (else
         (error "invalid expresssion" expr))))

;;;; Primitive environment
(define (make-environment alist)
  (list
   (map (lambda (pair)
          (cons (car pair) (box (cdr pair))))
        alist)))

(define scheme-report-environment-sic
  (make-environment
    `((cons       . ,cons)
      (car        . ,car)
      (cdr        . ,cdr)
      (+          . ,+)
      (=          . ,=)
      )))

;;;; Tests of the analysis and evaluation
(let ()
  (define ee (append (make-environment `((foo . 1) (bar . 2)))
                     scheme-report-environment-sic))
  (assert
   ;; sanity check
   ((analyze-self-eval 2) ee)                    => 2
   ((analyze-quoted '(quote foo)) ee)            => 'foo
   ;; variables
   ((analyze-variable 'foo) ee)                  => 1
   ((analyze-variable 'cdr) ee)                  => cdr
   ((analyze-set! '(set! bar 3)) ee)
   ((analyze-variable 'bar) ee)                  => 3
   ((analyze-define '(define baz 4)) ee)
   ((analyze-variable 'baz) ee)                  => 4
   ;; if
   ((analyze-if '(if 1 foo bar)) ee)             => 1
   ((analyze-if '(if 1 'foo bar)) ee)            => 'foo
   ((analyze-if '(if #f 'foo bar)) ee)           => 3
   ;; begin
   ((analyze-begun '(1)) ee)                     => 1
   ((analyze-begun '(1 2 3)) ee)                 => 3
   ((analyze-begun '((set! bar 4) (set! bar 5) bar)) ee)   => 5
   ;; lambda (just make sure we run make-proc)
   ((analyze-lambda '(lambda (x y) (+ x y))) ee)
   ;;
   ((analyze '+) ee)
   ((analyze '1) ee)
   ((analyze '2) ee)
   ((analyze-application '(+ 1 2)) ee)           => 3
   ((analyze-application '(= 4 4)) ee)           => #t
   ))

(define (evaluate environment expr)
  (let ((program (analyze-begun expr)))
    (program environment)))

(define (sic expr)
  (evaluate scheme-report-environment-sic expr))

(assert
 (sic
  '((define foo 1)
    (define bar 2)
    (define add
      (lambda (x y)
        (+ x y)))
    (add foo bar)))
 => 3
 )