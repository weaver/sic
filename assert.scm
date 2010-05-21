(define-syntax assert
  (syntax-rules (=>)
    ((_ e1 e2 ...)
     (and (assert e1)
          (assert e2 ...)))
    ((_ expr => val)
     (if (not (equal? expr val))
         (assert-error 'expr 'val)
         #t))
    ((_ expr)
     (if (not expr)
         (assert-error 'expr)
         #t))))

(define-syntax test
  (syntax-rules ()
    ((_ e1 e2 ...)
     (cons e2)
     )
    ))

(define-syntax assert1
  (syntax-rules ()
    ((_ expr)
     (if (not expr)
         (assert-error 'expr)
         #t))))

(define-syntax assert2
  (syntax-rules ()
    ((_ (expr val))
     (if (not (equal? expr val))
         (assert-error 'expr 'val)
         #t))))

(define (assert-error . stuff)
  (error
   (with-output-to-string
     (lambda ()
       (display "assertion failed:")
       (for-each (lambda (x)
                   (display " ")
                   (display x))
                 stuff)))))

(assert #t)
