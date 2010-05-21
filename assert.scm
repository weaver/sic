;; (define-syntax assert
;;   (syntax-rules (=>)
;;     ((_ e1 => v1 e2 ...)
;;      (and (assert e1 => v1) (assert e2 ...)))
;;     ((_ e1 e2 ...)
;;      (and (assert e1) (assert e2 ...)))
;;     ((_ expr => val)
;;      (if (not (equal? expr val))
;;          (assert-error 'expr "=>" 'val)
;;          #t))
;;     ((_ expr)
;;      (if (not expr)
;;          (assert-error 'expr)
;;          #t))))

(define-syntax assert
  (syntax-rules (=>)
    ((_ expr => val)
     (if (not (equal? expr val))
         (assert-error 'expr "=>" 'val)
         #t))
    ((_ e1 => v1 e2 ...)
     (and (assert e1 => v1) (assert e2 ...)))))

(define (assert-error . stuff)
  (error
   (with-output-to-string
     (lambda ()
       (display "assertion failed:")
       (for-each (lambda (x)
                   (display " ")
                   (write x))
                 stuff)))))

;; (assert
;;  1 => 1
;;  'foo => 'foo
;;  '(foo bar) => '(foo bar)
;;  1 => 2
;;  3 => 4)
