(define (atom? obj)
  (not (or (pair? obj)
           (null? obj))))

(define (inter-symbol env sym val)
  (cons (cons sym val)
        env))

(define (compile-literal env form)
  (if (null? form)
      '()
      (let ((head (car form)))
        (if (atom? head)
            (inter-symbol env head (compile-literal env (cdr form)))
            form))))

(assert
 (compile-literal '() '())
  => '()
 (compile-literal '() '(foo))
  => '((foo))
 (compile-literal '() '(foo (bar (baz zup))))
  => '((foo (bar (baz zup)))))

(define (compile-define env form)
  )

(define (compile-lambda env form)

  )

(define (compile form env code)
  (if (null? form)
      (create-runnable env code)
      (let ((head (car form))
            (tail (cdr form)))
        (cond ((eq 'quote head)
               (compile-literal tail))
              ((eq 'define head)
               (compile-define env tail))
              ((eq 'define-syntax head)
               (compile-syntax env tail))
              ((eq 'lambda head)
               (compile-lambda tail))
              ((eq 'if head)
               (compile-if tail))
              (else (error "Syntax Error"))))))

(define source0
  '(begin
     (lambda (x) x)))
