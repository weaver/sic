;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7
;;;; PLT r5rs compatibilty: (namespace-require 'r5rs)
;;;; Scheme48 compatibilty: ,open define-record-types

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

(define (foldr* proc nil lst car cdr null?)
  (if (null? lst)
      nil
      (proc (car lst)
            (foldr* proc nil (cdr lst) car cdr null?))))

(define (foldr proc nil lst)
  (foldr* proc nil lst car cdr null?))

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

(define (debug . args)
  (for-each (lambda (x)
              (display x)
              (display #\space))
            args)
  (display #\newline))

(define (error . args)
  (apply debug "error" args)
  (cdr 'error))

(define-record-type binding rtd/binding
  (bind sym val)
  binding?
  (sym bound)
  (val unbox set-box!))

(define-record-type frame rtd/frame
  (frame-cons car cdr)
  frame?
  (car binding1)
  (cdr frame-cdr))

(define (frame-null) frame-null)
(define (frame-null? obj) (eq? obj frame-null))

(define-record-type environment rtd/environment
  (env-cons car cdr)
  env?
  (car frame1 set-frame1!)
  (cdr env-cdr))

(define (env-null) env-null)
(define (env-null? obj) (eq? obj env-null))

(define (env-get-box-in-frame frame sym)
  (let lp ((frame frame))
    (if (frame-null? frame)
        #f
        (let ((binding (binding1 frame)))
          (if (eq? sym (bound binding))
              binding
              (lp (frame-cdr frame)))))))

(define (env-get-box env sym . undefined)
  (let lp ((env env))
    (if (env-null? env)
        (if (null? undefined)
            (error "undefined variable" sym)
            (car undefined))
        (if-let* ((box (env-get-box-in-frame (frame1 env) sym)))
                 box
                 (lp (env-cdr env))))))

(define (env-get env sym)
  (unbox (env-get-box env sym)))

(define (env-set! env sym val)
  (set-box! (env-get-box env sym) val))

(define (env-define! env sym val)
  (if-let* ((cur (env-get-box-in-frame (frame1 env) sym)))
           (set-box! cur val)
           (set-frame1! env (frame-cons (bind sym val) (frame1 env)))))

(define (env-extend env bindings values)
  (env-cons (foldr frame-cons
                   frame-null
                   (map bind bindings values))
            env))

(define (make-environment alist . tail)
  (env-cons (foldr (lambda (pair tail)
                     (frame-cons (bind (car pair) (cdr pair))
                                 tail))
                   frame-null
                   alist)
            (if (null? tail) env-null (car tail))))

(define (map-env proc lst)
  (foldr* (lambda (x acc)
            (cons (proc x) acc))
          '()
          lst
          frame1 env-cdr env-null?))

(define (map-frame proc lst)
  (foldr* (lambda (x acc)
            (cons (proc x) acc))
          '()
          lst
          binding1 frame-cdr frame-null?))

(define (inspect-environment env)
  (map-env (lambda (frame)
             (map-frame (lambda (binding)
                          (cons (bound binding) (unbox binding)))
                        frame))
           env))

(let ()
  (define env (make-environment '((a . 1) (b . 2))))
  (assert
   (env-get env 'a)                    => 1
   (env-get env 'b)                    => 2
   (env-define! env 'c 3)
   (env-get env 'c)                    => 3
   (env-set! env 'a 12)
   (env-get env 'a)                    => 12
   (set! env (env-extend env '(e f g) '(7 8 9)))
   (env-get env 'g)                    => 9
   (env-get env 'a)                    => 12
   ))

;;;; Types and expression predicates
(define-record-type proc rtd/proc
  (make-proc env body args)
  proc?
  (env proc-env)
  (body proc-body)
  (args proc-args))

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
(define prompt-tag cadr)
(define prompt-thunk caddr)

(define (abort? e) (tagged-list? e 'abort*))
(define abort-tag cadr)
(define abort-thunk caddr)

(define (capture? e) (tagged-list? e 'capture*))
(define capture-tag cadr)
(define capture-proc caddr)

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
    (if (prompt-obj? next)
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

(define (analyze-dynamic-call tag thunk build-k)
  (let ((tag (analyze tag)) (thunk (analyze thunk)))
    (lambda (env k)
      (tag env
           (cont k (k tag)
             (thunk env
                    (cont k (k thunk)
                      (execute-application
                       (build-k tag thunk k)
                       thunk
                       '()))))))))

(define (analyze-prompt e)
  (analyze-dynamic-call
   (prompt-tag e)
   (prompt-thunk e)
   (lambda (tag thunk k)
     (cons (make-prompt tag)
           k))))

(define (analyze-abort e)
  (analyze-dynamic-call
   (abort-tag e)
   (abort-thunk e)
   (lambda (tag thunk k)
     (let lp ((k k))
       (if (null? k)
           (error "end of continuation stack")
           (let ((head (car k)))
             (if (prompt-match? head tag)
                 (cdr k)
                 (lp (cdr k)))))))))

(define (analyze-capture e)
  (analyze-dynamic-call
   (capture-tag e)
   (capture-proc e)
   (lambda (tag thunk k)
     (let lp ((k k) (cap '()))
       (if (null? k)
           (error "end of continuation stack")
           (let ((head (car k)))
             (if (prompt-match? head tag)
                 cap
                 (lp (cdr k) (cons head cap)))))))))

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
        ((prompt? expr) (analyze-prompt expr))
        ((abort? expr) (analyze-abort expr))
        ((capture? expr) (analyze-capture expr))
        ;; primitives
        ((procedure? expr) (analyze-self-eval expr))
        ((pair? expr) (analyze-application expr))
        (else
         (error "invalid expresssion" expr))))

;;;; Primitive environment
(define scheme-report-environment-sic
  (make-environment
    `((cons       . ,cons)
      (car        . ,car)
      (cdr        . ,cdr)
      (null?      . ,null?)
      (+          . ,+)
      (=          . ,=)
      )))

;;;; Tests of the analysis and evaluation
(let ()
  (define ee (make-environment
               `((foo . 1) (bar . 2))
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

(let ()
  (define env scheme-report-environment-sic)
  (define (sic expr) (evaluate expr env))
  (sic
   '((define foo 1)
     (define bar 2)
     (define add (lambda (x y) (+ x y)))

     (define foldr
       (lambda (proc nil lst)
         (if (null? lst)
             nil
             (proc (car lst)
                   (foldr proc nil (cdr lst))))))

     (define map
       (lambda (proc lst)
         (foldr (lambda (x tail)
                  (cons (proc x) tail))
                '()
                lst)))

     (define delim1
       (lambda ()
         (prompt*
          'p1
          (lambda ()
            (abort*
             'p1
             (lambda () 1))))))

     (define delim2
       (lambda ()
         (prompt*
          'p1
          (lambda ()
            (map (lambda (x)
                   (abort*
                    'p1
                    (lambda () x)))
                 '(1 2 3))))))
     ))

  (assert
   (sic '((add foo bar)))                   => 3
   (sic '((map (lambda (x) x) '(1 2 3))))   => '(1 2 3)
   (sic '((delim1)))                        => 1
   (sic '((delim2)))                        => 3
   ))
