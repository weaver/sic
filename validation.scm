;;;; PLT r5rs compatibilty: (namespace-require 'r5rs)
;;;; Scheme48 compatibilty: ,open srfi-9 exceptions srfi-23 fluids

(load "utility.scm")
(load "scheme-backend.scm")
;; (load "90min.scm")

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
 )

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
