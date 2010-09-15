;;;; cps.scm -- Convert Scheme forms into continuation-passing style

,open srfi-23
(load "scheme-backend.scm")


;;; Utilities

(define (make-gensym default seed)
  (lambda prefix
    (set! seed (+ seed 1))
    (string->symbol
     (string-append
      (symbol->string (if (null? prefix) default (car prefix)))
      "."
      (number->string seed)))))

(define gensym (make-gensym 'g 0))

(let ()
  (assert
   (gensym) => 'g.1
   (gensym) => 'g.2
   (gensym 'k) => 'k.3))

(define (string-starts-with string prefix)
  (let ((slen (string-length string))
        (plen (string-length prefix)))
    (and (<= plen slen)
         (let loop ((ii 0))
           (or (eq? ii plen)
               (and (eq? (string-ref string ii)
                         (string-ref prefix ii))
                    (loop (+ ii 1))))))))

(define (symbol-starts-with symbol prefix)
  (string-starts-with
   (symbol->string symbol)
   (symbol->string prefix)))

(let ()
  (assert
   (symbol-starts-with '%foo '%)
   (symbol-starts-with 'bar '%) => #f))

(define (foldr kons seed data)
  (if (null? data)
      seed
      (kons (car data)
            (foldr kons seed (cdr data)))))

(define (foldl kons seed data)
  (if (null? data)
      seed
      (foldl kons
             (kons (car data) seed)
             (cdr data))))

(define (uniq seq)
  (foldl (lambda (item result)
           (if (memq item result)
               result
               (cons item result)))
         '()
         seq))

(let ()
  (assert
   (uniq '(a b a c d b e)) => '(e d c b a)))

(define (never? _) #f)


;;; Grammar

(define identifier? variable?)

;; Stub
(define (primitive? expr)
  (and (identifier? expr)
       (or (symbol-starts-with expr '%)
           (eq? expr 'if)
           (eq? expr 'set!))))

(define (atom? expr)
  (or (self-eval? expr)
      (quoted? expr)
      (identifier? expr)))

(define application? pair?)


;;; CPS Conversion

(define (cps expr)
  (cpc expr '*CONT*))

(define (cpc expr cont)
  ((cpc-converter expr) expr cont))

(define (cpc-converter expr)
  (cond ((atom? expr) cpc-atom)
        ((set? expr) cpc-set)
        ((if? expr) cpc-if)
        ((begin? expr) cpc-begin)
        ((lambda? expr) cpc-lambda)
        ((application? expr)
         (let ((op (application-proc expr)))
           (cond ((lambda? op) cpc-lambda-application)
                 ((primitive? op) cpc-primitive-application)
                 (else cpc-application))))
        (else (error "invalid expression" expr))))

(define (return cont expr)
  (if cont `(,cont ,expr) expr))

(define-syntax let-cpc
  (syntax-rules ()
    ((_ () body)
     body)
    ((_ ((name expr) bind ...) body)
     (let ((value expr))
       (if (atom? value)
           (let ((name value))
             (let-cpc (bind ...) body))
           (let ((name (gensym 'name)))
             (cpc
              value
              `(lambda (,name)
                 ,(let-cpc (bind ...) body)))))))))

;; [v K]
;; (K v)
(define (cpc-atom expr cont)
  (return cont expr))

;; [(set! v E1) K]
;; (lambda (r1) (K (set! v r1)))
(define (cpc-set expr cont)
  (let-cpc ((set-val (set-value expr)))
   (return cont `(set! ,(set-symbol expr) ,set-val))))

;; [(if E1 E2 E3) K]
;; [E1 (lambda (r1) (if r1 [E2 K] [E3 K]))]
(define (cpc-if expr cont)
  (let-cpc ((if-k cont)
            (if-pred (if-predicate expr)))
   `(if ,if-pred
         ,(cpc (if-then expr) if-k)
         ,(cpc (if-else expr) if-k))))

;; [(begin E1 E2) K]
;; [E1 (lambda (r1) [E2 K])]
(define (cpc-begin expr cont)
  (let ((body (begin-body expr)))
    (if (null? body)
        (return cont 'c/undefined)
        (cpc-foldr body cont))))

;; [(lambda (p1..pn) E0) K]
;; (K (lambda (k p1..pn) [E0 k]))
(define (cpc-lambda expr cont)
  (let ((body (lambda-body expr)))
    (if (null? body)
        (error 'empty-lambda-body expr)
        (let ((kn (gensym 'k)))
          (return cont
                  `(lambda (,kn ,@(lambda-formals expr))
                     ,(cpc-foldr body kn)))))))

(define (cpc-foldr seq cont)
  (let ((expr (car seq))
        (tail (cdr seq)))
    ;; Optimize for the case where an expression in the sequence is
    ;; just an atom.  If it's not in the tail position, it can be
    ;; folded out.
    (if (and (atom? expr)
             (not (null? tail)))
        (cpc-foldr tail cont)
        (cpc expr
             (if (null? tail)
                 cont
                 `(lambda (,(gensym 'seq))
                    ,(cpc-foldr tail cont)))))))

;; [(+ E1 E2) K]
;; [E1 (lambda (r1) [E2 (lambda (r2) (K (+ r1 r2)))])]
(define (cpc-primitive-application expr cont)
  (let loop ((args (application-args expr))
             (names '()))
    (if (null? args)
        (return cont `(,(application-proc expr) ,@(reverse names)))
        (let-cpc ((val (car args)))
         (loop (cdr args) (cons val names))))))

;; [(E0 E1 E2) K]
;; [E0 (lambda (r0) [E1 (lambda (r1) [E2 (lambda (r2) (r0 K r1 r2))])])]
(define (cpc-application expr cont)
  (let-cpc ((op (application-proc expr)))
   (let loop ((args (application-args expr))
              (names '()))
     (if (null? args)
         `(,op ,cont ,@(reverse names))
         (let-cpc ((arg (car args)))
          (loop (cdr args)
                (cons arg names)))))))

;; [((lambda (P1 P2) E0) E1 E2) K]
;; [E1 (lambda (P1) [E2 (lambda (P2) [E0 K])])]
(define (cpc-lambda-application expr cont)
  (let* ((op (application-proc expr))
         (body (lambda-body op)))
    (if (null? body)
        (error 'empty-lambda-body op)
        (let loop ((formals (lambda-formals op))
                   (args (application-args expr)))
          (cond ((null? formals)
                 (if (not (null? args))
                     (error 'too-many-arguments expr)
                     (cpc-foldr body cont)))
                ((null? args)
                 (error 'too-few-arguments expr))
                (else
                 (cpc (car args)
                      `(lambda (,(car formals))
                         ,(loop (cdr formals)
                                (cdr args))))))))))

(define (test-cps expr)
  (set! gensym (make-gensym 'g 0))
  (cps expr))

(let ()
  (assert
   ;; cpc-atom
   (test-cps 'a)
   => '(*CONT* a)

   ;; cpc-set
   (test-cps '(set! a 1))
   => '(*cont* (set! a 1))
   (test-cps '(set! a (+ 1 2)))
   =>  '(+ (lambda (set-val.1) (*cont* (set! a set-val.1))) 1 2)

   ;; cpc-if
   (test-cps '(if v #t #f))
   => '(if v (*cont* #t) (*cont* #f))
   (test-cps '(if (maybe? foo) #t #f))
   => '(maybe? (lambda (if-pred.1) (if if-pred.1 (*cont* #t) (*cont* #f))) foo)

   ;; cpc-begin
   (test-cps '(begin a b c))
   => '(*cont* c)
   (test-cps '(begin (%foo) bar))
   => '((lambda (seq.1) (*cont* bar)) (%foo))

   ;; cpc-lambda
   (test-cps '(lambda (a) b c))
   => '(*cont* (lambda (k.1 a) (k.1 c)))
   (test-cps '(lambda (a) (%foo) bar))
   => '(*cont* (lambda (k.1 a) ((lambda (seq.2) (k.1 bar)) (%foo))))

   ;; cpc-primitive-application
   (test-cps '(%jump))
   => '(*cont* (%jump))
   (test-cps '(%~ 1))
   => '(*cont* (%~ 1))
   (test-cps '(%+ 1 (foo)))
   => '(foo (lambda (val.1) (*cont* (%+ 1 val.1))))

   ;; cpc-application
   (test-cps '(a))
   => '(a *cont*)
   (test-cps '(a b c))
   => '(a *cont* b c)
   (test-cps '(d (a b c) 2))
   => '(a (lambda (arg.1) (d *cont* arg.1 2)) b c)

   ;; cpc-lambda-application
   (test-cps '((lambda () a)))
   => '(*cont* a)
   (test-cps '((lambda (a) b) c))
   => '((lambda (a) (*cont* b)) c)
   (test-cps '((lambda (a b) c) d e))
   => '((lambda (a) ((lambda (b) (*cont* c)) e)) d)

   ;; larger example
   (test-cps `(begin
                (set! square (lambda (x) (%* x x)))
                (%+ (square 5) 1)))
   => '((lambda (set-val.3)
          ((lambda (seq.1)
             (square (lambda (val.2)
                       (*cont* (%+ val.2 1)))
                     5))
           (set! square set-val.3)))
        (lambda (k.4 x)
          (k.4 (%* x x))))
   ))


;;; Closure Conversion

(define (close expr env)
  (convert-closure expr never? env))

(define (convert-closure expr bound? free)
  ((closure-converter expr) expr bound? free))

(define (closure-converter expr)
  (cond ((atom? expr) close-atom)
        ((lambda? expr) close-lambda)
        ((application? expr)
         (let ((op (application-proc expr)))
           (cond ((primitive? op) close-primitive-application)
                 ((lambda? op) close-lambda-application)
                 (else close-application))))
        (else (error 'cannot-convert-closure expr))))

(define (close-sequence seq bound? free)
  (map (lambda (item)
         (convert-closure item bound? free))
       seq))

(define (bound-predicate names)
  (lambda (name)
    (memq name names)))

(define (closure-env-bind name value env)
  (cons (cons name value) env))

(define (closure-env-ref name bound? free)
  (cond ((bound? name) name)
        ((assq name free) => cdr)
        (else (error 'unbound-identifier name))))

;; An unbound identifier is converted to a %CLOSURE-REF.  Other atoms
;; are converted to themselves.
(define (close-atom expr bound? free)
  (if (identifier? expr)
      (closure-env-ref expr bound? free)
      expr))

;; Only arguments are converted in a primitive procedure application.
(define (close-primitive-application expr bound? free)
  `(,(application-proc expr)
    ,@(close-sequence (application-args expr) bound? free)))

;; LAMBDA applications are treated as binding forms (there's no
;; primitive LET).  Don't convert the LAMBDA to a closure.  Lift free
;; identifiers.  Convert the lambda body in the context of its lifted
;; formal parameters.  Convert the arguments in the current context.
;;
;; Note: Lifting makes the code verbose.  It might not be necessary.
;; Revisit this after making a code emitter.  Feeley uses a primitive
;; LET form.
(define (close-lambda-application expr bound? free)
  (let* ((op (application-proc expr))
         (free-in-expr (free-identifiers op))
         ;; Lifting is accomplished with these appends.
         (formals (append (lambda-formals op) free-in-expr))
         (args (append (application-args expr) free-in-expr)))
    `((lambda ,formals
        ,@(close-sequence
           (lambda-body op)
           (bound-predicate formals)
           '()))
      ,@(close-sequence args bound? free))))

;; A normal application is converted by looking up the procedure body
;; in the operator's closure and applying it to the operator along
;; with the converted arguments.
(define (close-application expr bound? free)
  (let ((op (convert-closure (application-proc expr) bound? free)))
    `((%closure-ref ,op 0)
      ,op
      ,@(close-sequence (application-args expr) bound? free))))

;; A LAMBDA is converted to a closure.  The body is scanned for free
;; identifiers that are bound into the closure along with the
;; procedure body.  The body is converted in the context of the
;; LAMBDA's formal parameters and the new closure.
(define (close-lambda expr bound? free)
  (let* ((self (gensym 'self))
         (formals (lambda-formals expr))
         (free-in-expr (free-identifiers expr)))
    `(%closure
      (lambda (,self ,@formals)
        ,@(close-sequence
           (lambda-body expr)
           (bound-predicate formals)
           (self-refs self free-in-expr)))
      ,@(map (lambda (name)
               (closure-env-ref name bound? free))
             free-in-expr))))

(define (self-refs self free)
  (let loop ((ii 1) (names free) (env '()))
    (if (null? names)
        env
        (loop (+ ii 1)
              (cdr names)
              (closure-env-bind
               (car names)
               `(%closure-ref ,self ,ii)
               env)))))

(define (free-identifiers expr)
  (uniq
   (let find ((tree expr) (bound '()) (free '()))
     (cond ((null? tree) free)
           ((identifier? tree)
            (cond ((primitive? tree) free)
                  ((memq tree bound) free)
                  (else (cons tree free))))
           ((atom? tree) free)
           ((lambda? tree)
            (find (lambda-body tree)
                  (append (lambda-formals tree) bound)
                  free))
           (else
            (find (cdr tree)
                  bound
                  (find (car tree) bound free)))))))

(define (test-close expr bound? free)
    (set! gensym (make-gensym 'g 0))
    (convert-closure expr bound? free))

(let ()
  (assert
   ;; Auxilliary
   (free-identifiers '(lambda (a) (+ a b c)))
   => '(+ b c)
   (free-identifiers '(lambda (a) (z (lambda (y) (a x y z)))))
   => '(x z)
   (self-refs 'self.1 '(a b))
   => '((b %closure-ref self.1 2) (a %closure-ref self.1 1))

   ;; Free identifier
   (test-close 'a never? (self-refs 'global '(a)))
   => '(%closure-ref global 1)
   ;; Bound identifier
   (test-close 'a (bound-predicate '(a)) '())
   => 'a
   ;; Closed identifier
   (test-close 'a never? (self-refs 'self.1 '(a)))
   => '(%closure-ref self.1 1)

   ;; Application: operator is free
   (test-close '(a c) (bound-predicate '(c)) (self-refs 'global '(a)))
   => '((%closure-ref (%closure-ref global 1) 0) (%closure-ref global 1) c)
   ;; Application: some operands are bound
   (test-close '(a c d) (bound-predicate '(a d)) (self-refs 'global '(c)))
   => '((%closure-ref a 0) a (%closure-ref global 1) d)

   ;; Lambda
   (test-close '(lambda (a) (%+ a b)) (bound-predicate '(b)) '())
   => '(%closure
        (lambda (self.1 a) (%+ a (%closure-ref self.1 1)))
        b)

   ;; Application: lambda as operator
   (test-close '((lambda (a b) (%+ a b 1)) c d)
               (bound-predicate '(c))
               (self-refs 'global '(d)))
   => '((lambda (a b) (%+ a b 1)) c (%closure-ref global 1))

   ;; Large example
   (test-close
    (test-cps
     `(begin
        (set! square (lambda (x) (%* x x)))
        (%+ (square 5) 1)))
    never?
    '((square . square)
      (*cont* . *cont*)))
   => '((lambda (set-val.3 *cont* square)
          ((lambda (seq.1 square *cont*)
             ((%closure-ref square 0)
              square
              (%closure
               (lambda (self.1 val.2)
                 ((%closure-ref (%closure-ref self.1 1) 0)
                  (%closure-ref self.1 1)
                  (%+ val.2 1)))
               *cont*)
              5))
           (set! square set-val.3)
           square
           *cont*))
        (%closure
         (lambda (self.2 k.4 x)
           ((%closure-ref k.4 0)
            k.4
            (%* x x))))
        *cont*
        square)

   ;; Large example
   (test-close
    (test-cps
     '(lambda (x y z)
        ((lambda (f)
           (%- (f 1 2) (f 3 4)))
         (lambda (a b)
           (%+ (%* a x) (%* b y))))))
    never?
    '((*cont* . *cont*)))
   => '((%closure-ref *cont* 0)
        *cont*
        (%closure
         (lambda (self.1 k.1 x y z)
           ((lambda (f k.1)
              ((%closure-ref f 0)
               f
               (%closure
                (lambda (self.2 val.2)
                  ((%closure-ref (%closure-ref self.2 1) 0)
                   (%closure-ref self.2 1)
                   (%closure
                    (lambda (self.3 val.3)
                      ((%closure-ref (%closure-ref self.3 1) 0)
                       (%closure-ref self.3 1)
                       (%- (%closure-ref self.3 2) val.3)))
                    (%closure-ref self.2 2)
                    val.2)
                   3
                   4))
                f
                k.1)
               1
               2))
            (%closure
             (lambda (self.4 k.4 a b)
               ((lambda (val.5 k.4 b y)
                  ((lambda (val.6 k.4 val.5)
                     ((%closure-ref k.4 0)
                      k.4
                      (%+ val.5 val.6)))
                   (%* b y)
                   k.4
                   val.5))
                (%* a (%closure-ref self.4 2))
                k.4
                b
                (%closure-ref self.4 1)))
             y
             x)
            k.1))))
   ))


;;; Primitive Evaulation

(define %closure vector)
(define %closure-ref vector-ref)
(define %+ +)
(define %- -)
(define %* *)
(define %/ /)

(define (evaluate expr env)
  (eval
   `(call-with-current-continuation
     (lambda (return)
       ;; Reify the real continuation as a %CLOSURE.  The %CLOSURE
       ;; body discards self and applies the continuation values to
       ;; the real continuation.
       (let ((*cont* (%closure (lambda (self . rest) (apply return rest)))))
         ,(close (cps expr)
                 `((*cont* . *cont*) ,@env)))))
   (interaction-environment)))

(let ()
  (assert
   (evaluate ((lambda (a) (%* a a)) 5) '())
   => 25))

