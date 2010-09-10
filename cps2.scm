(define (fold-right-penultimate proc last lst)
  (let lp ((lst lst))
    (if (null? (cdr lst))               ; you'll get an error on an empty list
        (last (car lst))
        (proc (car lst) (lp (cdr lst))))))

(define (foldl proc nil lst)
  (if (null? lst)
      nil
      (foldl proc
             (proc (car lst) nil)
             (cdr lst))))

(define (cps-begin . body)
  `(lambda (cont)
     ,(foldr (lambda (head tail)
               `(,head (lambda (v)
                         ,tail)))
             `(cont v)
             body)))

(assert
 (cps-begin 1 2) =>
 '(lambda (cont)
    (1 (lambda (v)
         (2 (lambda (v)
              (cont v))))))
 )

(define-syntax begin1
  (syntax-rules ()
    ((_ first rest ...)
     (let ((result first))
       rest ...
       result))))

(define (cps-arguments . body)
  (let* ((symbols (map (lambda (x) (gensym)) body))
         (cur symbols)
         (sym (lambda ()
                (begin1
                 (car cur)
                 (set! cur (cdr symbols))))))
    `(lambda (cont)
       ,(foldl (lambda (head tail)
                 `(,head (lambda (,(sym))
                           ,tail)))
               `(cont (list ,@symbols))
               body))))

(assert
 (cps-arguments 1 2) =>
 '(lambda (cont)
    (1 (lambda (v1)
         (2 (lambda (v2)
              (cont (list v1 v2))))))
    )
 )



'(lambda (k)
   (1 (cont k (k v)
        ((lambda (k) (2 (cont k (k v) (return k v))))
         k))
      ))

(define (cps-lambda formal . body)
  `(lambda (k)

     )
  )

(define module-sic
  `((lambda . ,(define )
        ))
  )
