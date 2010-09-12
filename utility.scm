;;;; Utility
(define-syntax assert
  (syntax-rules (=>)
    ;; arrow is an infix operator for equal?
    ((_ expr => val)
     (let ((result expr))
       (if (not (equal? result val))
           (error "assertion failed" 'expr '=> result 'not val)
           #t)))
    ;; optional arrow and value
    ((_ expr)
     (if (not expr)
         (error "assertion failed" 'expr)
         #t))
    ;; recursion with and without the arrow
    ((_ e1 => v1 e2 ...) (and (assert e1 => v1) (assert e2 ...)))
    ((_ e1 e2 ...) (and (assert e1) (assert e2 ...)))))

(define-syntax if-let1
  (syntax-rules ()
    ((_ ((sym expr)) then else)
     (let ((sym expr))
       (if sym then else)))
    ((_ ((expr)) then else)
     (if expr then else))))

(define-syntax if-let*
  (syntax-rules ()
    ;; provide a default else clause
    ((_ bindings then) (if-let* bindings then #f))
    ((_ () then else) (begin then))
    ((_ (b1) then else) (if-let1 (b1) then else))
    ((_ (b1 b2 ...) then else)
     (if-let1 (b1)
              (if-let* (b2 ...) then else)
              else))))

(define (foldr* proc nil lst car cdr null?)
  (if (null? lst)
      nil
      (proc (car lst)
            (foldr* proc nil (cdr lst) car cdr null?))))

(define (foldr proc nil lst)
  (foldr* proc nil lst car cdr null?))

(assert
 (if-let1 ((#f)) 1 2)             => 2
 (if-let1 ((foo 1)) foo 2)        => 1
 (if-let* ((#f)) 1 2)             => 2
 (if-let* ((foo 1)) foo 2)        => 1
 (if-let* ((foo 1) (bar 1)) (+ foo bar) #f)          => 2
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar) #f)  => #f
 (if-let* ((foo 1) (bar (= foo 2))) (+ foo bar))     => #f
 (foldr cons '() '(1 2 3))        => '(1 2 3)
 )

(define-syntax begin1
  (syntax-rules ()
    ((_ first rest ...)
     (let ((result first))
       rest ...
       result))))

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
