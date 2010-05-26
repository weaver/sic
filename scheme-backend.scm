(define (atom? obj)
  (not (or (pair? obj)
           (null? obj))))

(define (pdefine env sym)
  (set-car! env (cons (list sym *undefined*) (car env)))
  env)

(define (pbind env sym)
  (cons (list (list sym))
        env))

(define (plookup env sym)
  (call/cc
   (lambda (return)
     (map (lambda (frame)
            (map (lambda (pair)
                   (if (eq (car pair) sym)
                       (return pair)))
                 frame))
          env)))
  'lookup-error)

(define (pset! env sym val)
  (let (pair (plookup env sym))
    (set-cdr! pair (list val))
    env))

(define (pblock env body)
  )

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
