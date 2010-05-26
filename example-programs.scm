;; Example 1
(+ 1 2)

;; Emit 1
(%add (i32 1) (i32 2))

;; Example 2
(define (++ a)
  (+ a 1))

(++ 2)

;; Primitive 2
(pdefine ENV '++)
(set-env! ENV '++ (compile-lambda '(lambda (a) (+ a 1))))
(pset! ENV (pbody
            (pbind ENV 'a)
            (pbody '+ a 1)))


(pbind ENV 'a)
(pbody ENV )

;; Emit 2
++:
  pop l1
  add l1, 1
  push l1
push 2
goto ++
pop res




;; Example 2.5
(define counter
  ((lambda (value)
     (lambda ()
       (set! value (++ value))
       value))
   0))

(define foo (counter))
(foo)

;; Example 3
(define (map fn data)
  (if (null? data)
      '()
      (cons (fn (car data))
            (map fn (cdr data)))))

(map inc '(1 2 3))

;; Example 4, something with shift/reset