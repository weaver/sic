;;;; cps.scm -- Convert Scheme forms into continuation-passing style

;; ,open srfi-23
(load "scheme-backend.scm")


;;; Utilities

(define gensym
  (let ((unique 0))
    (lambda prefix
      (set! unique (+ unique 1))
      (string->symbol
       (string-append
        (symbol->string (if (null? prefix) 'g: (car prefix)))
        (number->string unique))))))

(let ()
  (assert
   (gensym) => 'g:1
   (gensym) => 'g:2
   (gensym 'k:) => 'k:3))


;;; Conversion

(define (cps expr)
  (cpc expr '() '*CONT*))

(define (cpc expr env cont)
  ((converter expr) expr env cont))

(define (converter expr)
  (cond ((atom? expr) cpc-atom)
        ((set? expr) cpc-set)
        ((if? expr) cpc-if)
        ((begin? expr) cpc-begin)
        ((lambda? expr) cpc-lambda)
        ((application? expr)
         (if (lambda? (application-proc expr))
             cpc-lambda-application
             cpc-application))
        (else (error "invalid expression" expr))))

(define (return cont expr)
  (if cont `(,cont ,expr) expr))

(define (atom? expr)
  (or (self-eval? expr)
      (variable? expr)))

(define application? pair?)

;; [v K]
;; (K v)
(define (cpc-atom expr env cont)
  (return cont expr))

;; [(set! v E1) K]
;; (lambda (r1) (K (set! v r1)))
(define (cpc-set expr env cont)
  (let ((vn (gensym 'v:)))
    (cpc
     (set-value expr)
     env
     `(c/lambda (,vn)
        ,(return cont `(c/set! ,(set-symbol expr) ,vn))))))

;; [(if E1 E2 E3) K]
;; [E1 (lambda (r1) (if r1 [E2 K] [E3 K]))]
(define (cpc-if expr env cont)
  (let ((pn (gensym 'p:))
        (kn (gensym 'k:)))
    `((c/lambda (,kn)
        ,(cpc (if-predicate expr)
              env
              `(c/lambda (,pn)
                 (c/if ,pn
                       ,(cpc (if-then expr) env kn)
                       ,(cpc (if-else expr) env kn)))))
      ,cont)))

;; [(begin E1 E2) K]
;; [E1 (lambda (r1) [E2 K])]
(define (cpc-begin expr env cont)
  (let ((body (begin-body expr)))
    (if (null? body)
        (return cont 'c/undefined)
        (cpc-sequence body env cont))))

(define (cpc-sequence seq env cont)
  (let ((tail (cdr seq)))
    (cpc (car seq)
         env
         (if (null? tail)
             cont
             (let ((vn (gensym 'v:)))
               `(c/lambda (,vn)
                  ,(cpc-sequence (cdr seq) env cont)))))))

;; Feeley defines (primitive?) transform:
;; [(+ E1 E2) K]
;; [E1 (lambda (r1) [E2 (lambda (r2) (K (+ r1 r2)))])]

;; [(lambda (p1..pn) E0) K]
;; (K (lambda (k p1..pn) [E0 k]))
(define (cpc-lambda expr env cont)
  (let ((kn (gensym 'k:)))
    (return cont
            `(c/lambda (,kn ,@(lambda-formals expr))
               ,(cpc-sequence
                 (lambda-body expr)
                 env
                 kn)))))

;; [(E0 E1 E2) K]
;; [E0 (lambda (r0) [E1 (lambda (r1) [E2 (lambda (r2) (r0 K r1 r2))])])]
(define (cpc-application expr env cont)
  (cpc (application-proc expr)
       env
       (let loop ((args (application-args expr))
                  (names (list (gensym 'o:))))
         `(c/lambda (,(car names))
            ,(if (null? args)
                 (let ((names (reverse names)))
                   `(,(car names) ,cont ,@(cdr names)))
                 (cpc (car args)
                      env
                      (loop (cdr args)
                            (cons (gensym 'a:) names))))))))

;; [((lambda (P1 P2) E0) E1 E2) K]
;; [E1 (lambda (P1) [E2 (lambda (P2) [E0 K])])]
(define (cpc-lambda-application expr env cont)
  (let* ((op (application-proc expr))
         (body (lambda-body op)))
    (let loop ((formals (lambda-formals op))
               (args (application-args expr)))
      (if (null? formals)
          (cpc-sequence body env cont)
          (cpc (car args)
               env
               `(c/lambda (,(car formals))
                          ,(loop (cdr formals)
                                 (cdr args))))))))

(let ()
  (assert
   ;; cpc-atom
   (cps 'a) => '(*CONT* a)
   ;; cpc-set
   (cps '(set! a 1))
   ;; cpc-if
   (cps '(if v #t #f))
   ;; cpc-begin
   (cps '(begin a b c))
   ;; cpc-lambda
   (cps '(lambda (a) b c))
   ;; cpc-application
   (cps '(a))
   (cps '(a b))
   (cps '(a b c))
   ;; cpc-lambda-application
   (cps '((lambda () a)))
   (cps '((lambda (a) b) c))
   (cps '((lambda (a b) c) d e))

   (p (cps `(begin
              (set! square (lambda (x) (* x x)))
              (+ (square 5) 1))))
   ))

(define-syntax c/lambda
  (syntax-rules ()
    ((_ . rest) (lambda . rest))))

(define-syntax c/if
  (syntax-rules ()
    ((_ . rest) (if . rest))))

(define-syntax c/set
  (syntax-rules ()
    ((_ . rest) (set! . rest))))

(define-syntax c/define
  (syntax-rules ()
    ((_ . rest) (define . rest))))

(define c/undefined '*undefined*)

(define (call/cc k f)
  (f k (lambda (_ result)
         (k result))))