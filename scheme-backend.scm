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

(define (foldr proc nil lst)
  (if (null? lst)
      nil
      (proc (car lst)
            (foldr proc nil (cdr lst)))))

(assert
 (if-let1 ((#f)) 1 2)             => 2
 (if-let1 ((foo 1)) foo 2)        => 1
 (if-let* ((#f)) 1 2)             => 2
 (if-let* ((foo 1)) foo 2)        => 1
 (if-let* ((foo 1) (bar 1)) (+ foo bar) #f)          => 2
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar) #f)  => #f
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar))     => #f
 (foldr cons '() '(1 2 3))        => '(1 2 3)
 )

;;;; Environment
(define unspecified "Unspecified Value")
(define error raise)

;; Boxes have actually been eliminated now, so box is simply
;; identity. Instead of a separate value box, we return the binding
;; pair with its name, and set-cdr! on the pair.
(define (box val) val)
(define (unbox box) (cdr box))
(define (set-box! box val) (set-cdr! box val))

(define (env-get-box-in-frame frame sym)
  (let lp ((frame frame))
    (if (null? frame)
        #f
        (let ((binding (car frame)))
          (if (eq? sym (car binding))
              binding
              (lp (cdr frame)))))))

(define (env-get-box env sym . undefined)
  (let lp ((env env))
    (if (null? env)
        (if (null? undefined)
            (error "undefined variable" sym)
            (car undefined))
        (if-let* ((box (env-get-box-in-frame (car env) sym)))
                 box
                 (lp (cdr env))))))

(define (env-get env sym)
  (unbox (env-get-box env sym)))

(define (env-set! env sym val)
  (set-box! (env-get-box env sym) val))

(define (env-define! env sym val)
  (if-let* ((cur (env-get-box-in-frame (car env) sym)))
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

(let ()
  ;; The initial environment is two frames. The first with `a' bound
  ;; to `1', and the second with `b' bound to `2'. Using env-define!
  ;; should only search the first frame for a binding.
 (define (inspect-env env) env)
 (define env `(((a . ,(box 1))) ((b . ,(box 2)))))
 (assert
  (inspect-env env) => '(((a . 1)) ((b . 2)))
  (env-define! env 'b 3)
  (inspect-env env) => '(((b . 3) (a . 1)) ((b . 2)))
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

(define (prompt? e) (tagged-list? e 'prompt*))
(define (prompt-tag e) cadr)
(define (prompt-thunk e) caddr)

(define (abort? e) (tagged-list? e 'abort*))
(define (abort-tag e) cadr)
(define (abort-thunk e) caddr)

(define (capture? e) (tagged-list? e 'capture*))
(define (capture-tag e) cadr)
(define (capture-proc e) caddr)

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

;;;; Dynamic
(define-syntax cont
  (syntax-rules ()
    ((_ k formal body ...)
     (cons (lambda formal body ...)
           k))))

(define (return k val)
  (let ((next (car k)))
    (if (prompt-obj? k)
        (return (cdr k) val)
        (next (cdr k) val))))

(define (make-prompt tag)
  (vector 'prompt tag))

(define (prompt-obj? obj)
  (and (vector? obj) (eq? (vector-ref obj 0) 'prompt)))

(define (prompt-match? prompt tag)
  (and (prompt-obj? prompt) (eq? tag (vector-ref prompt 1))))

;;;; Analysis
(define (analyze-self-eval e)
  (lambda (env k) (return k e)))

(define (analyze-quoted e)
  (let ((q (quote-body e)))
    (lambda (env k) (return k q))))

(define (analyze-variable e)
  (lambda (env k)
    (return k (env-get env e))))

(define (analyze-define e)
  (let ((var (define-symbol e))
        (val (analyze (define-value e))))
    (lambda (env k)
      (val env
           (cont k (k v)
             (env-define! env var v)
             (return k unspecified))))))

(define (analyze-set! e)
  (let ((var (set-symbol e))
        (val (analyze (set-value e))))
    (lambda (env k)
      (val env
           (cont k (k v)
             (env-set! env var v)
             (return k unspecified))))))

(define (analyze-prompt e)
  (let ((tag (analyze (prompt-tag e)))
        (thunk (analyze (prompt-thunk e))))
    (lambda (env k)
      (tag env
           (cont k (k tag)
             (thunk env
                    (cons (make-prompt tag)
                          k)))))))

(define (analyze-abort e)
  (define (search k tag)
    (let lp ((k k))
      (if (null? k)
          (error "end of continuation stack")
          (let ((head (car k)))
            (if (prompt-match? head tag)
                k
                (lp (cdr k)))))))
  (let ((tag (analyze (abort-tag e)))
        (thunk (analyze (abort-thunk e))))
    (lambda (env k)
      (tag env
           (cont k (k tag)
             (thunk env
                    (search k tag)))))))

(define (analyze-capture e)
  (define (search k tag)
    (let lp ((k k) (cap '()))
      (if (null? k)
          (error "end of continuation stack")
          (let ((head (car k)))
            (if (prompt-match? head tag)
                cap
                (lp (cdr k) (cons head cap)))))))
  (let ((tag (analyze (capture-tag e)))
        (proc (analyze (capture-proc e))))
    (lambda (env k)
      (tag env
           (cont k (k tag)
             (execute-application
              k
              proc
              (list (search k tag))))))))

(define (analyze-if e)
  (let ((pred? (analyze (if-predicate e)))
        (then  (analyze (if-then e)))
        (else  (analyze (if-else e))))
    (lambda (env k)
      (pred? env
             (cont k (k v)
               (if v (then env k) (else env k)))))))

(define (analyze-lambda e)
  (let ((syms (lambda-formals e))
        (body (analyze-begun (lambda-body e))))
    (lambda (env k)
      (return k (make-proc env body syms)))))

(define (analyze-begin e)
  (analyze-begun (begin-body e)))

(define (make-sequence handler)
  (lambda (e1 e2)
    (lambda (env k)
      (e1 env
          (cont k (k v1)
            (e2 env
                (cont k (k v2)
                  (handler k v1 v2))))))))

(define sequence-begin
  (make-sequence (lambda (k v1 v2)
                   (return k v2))))

(define (analyze-begun exprs)
  (define (lp head tail)
    (if (null? tail)
        head
        (lp (sequence-begin head (car tail))
            (cdr tail))))
  (if (null? exprs)
      (error "empty body")
      (lp (analyze (car exprs))
          (map analyze (cdr exprs)))))

(define debug debug-message)

(define sequence-apply
  (make-sequence (lambda (k v1 v2)
                   (return k (cons v1 v2)))))

(define (null-expression env k)
  (return k '()))

(define (analyze-application e)
  (let ((proc (analyze (application-proc e)))
        (args (foldr sequence-apply
                     null-expression
                     (map analyze (application-args e)))))
    (lambda (env k)
      (args env
            (cont k (k args)
              (proc env
                    (cont k (k proc)
                      (execute-application k proc args))))))))

(define (execute-application k proc args)
  (cond ((procedure? proc)
         (return k (apply proc args)))
        ((proc? proc)
         ((proc-body proc)
          (env-extend (proc-env proc)
                      (proc-args proc)
                      args)
          (cont k (k v) (return k v))))
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
  ;; this just returns the alist now, since box is identity.
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
  (define value #f)
  (define kk (list (lambda (k v) (set! value v) value)))

   ;; sanity check
  (assert
   ((analyze-self-eval 2) ee kk)                    => 2
   ((analyze-quoted '(quote foo)) ee kk)            => 'foo
   )
  ;; variables
  (assert
   ((analyze-variable 'foo) ee kk)                  => 1
   ((analyze-variable 'cdr) ee kk)                  => cdr
   ((analyze-set! '(set! bar 3)) ee kk)
   ((analyze-variable 'bar) ee kk)                  => 3
   ((analyze-define '(define baz 4)) ee kk)
   ((analyze-variable 'baz) ee kk)                  => 4
   )
  ;; ;; if
  (assert
   ((analyze-if '(if 1 foo bar)) ee kk)             => 1
   ((analyze-if '(if 1 'foo bar)) ee kk)            => 'foo
   ((analyze-if '(if #f 'foo bar)) ee kk)           => 3
   )
  ;; ;; begin
  (assert
   ((analyze-begun '(1)) ee kk)                     => 1
   ((analyze-begun '(1 2 3)) ee kk)                 => 3
   ((analyze-begun '((set! bar 4) (set! bar 5) bar)) ee kk)   => 5
   )
  ;; ;; lambda (just make sure we run make-proc)
  (assert
   ((analyze-lambda '(lambda (x y) (+ x y))) ee kk))
  ;; ;; application
  (assert
   ((analyze '+) ee kk)
   ((analyze '1) ee kk)
   ((analyze '2) ee kk)
   ((analyze-application '(+ 1 2)) ee kk)           => 3
   ((analyze-application '(= 4 4)) ee kk)           => #t
   )
  )

(define (evaluate expr environment)
  (let ((value #f))
    ((analyze-begun expr)
     environment
     (list (lambda (k v)
             (set! value v))))
    value))

(define (sic expr)
  (evaluate expr scheme-report-environment-sic))

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
