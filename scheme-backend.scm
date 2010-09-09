;;;; Analyzing interpreter. See
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1.7

;;;; Environment
(define unspecified "Unspecified Value")

(define (debug . args)
  (for-each (lambda (x)
              (display x)
              (display #\space))
            args)
  (display #\newline))

(define (error . args)
  (raise args)
  ;; (apply debug "error" args)
  ;; (cdr 'error)
  )

(define-record-type rtd/binding
  (bind sym val)
  binding?
  (sym bound)
  (val unbox set-box!))

(define-record-type rtd/frame
  (frame-cons car cdr)
  frame?
  (car binding1)
  (cdr frame-cdr))

(define (frame-null) frame-null)
(define (frame-null? obj) (eq? obj frame-null))

(define-record-type rtd/environment
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

;;;; Types and expression predicates
(define-record-type rtd/proc
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
