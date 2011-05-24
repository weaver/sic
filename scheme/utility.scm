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
    ((_ () then else) (begin then))
    ((_ (b1) then else) (if-let1 (b1) then else))
    ((_ (b1 b2 ...) then else)
     (if-let1 (b1)
              (if-let* (b2 ...) then else)
              else))))

(define-syntax and-let*
  (syntax-rules ()
    ((_ bindings body) (if-let* bindings then #f))))

(define (foldr* proc nil lst car cdr null?)
  (if (null? lst)
      nil
      (proc (car lst)
            (foldr* proc nil (cdr lst) car cdr null?))))

(define (foldr-one proc nil lst)
  (foldr* proc nil lst car cdr null?))

(define (foldr-two proc nil l1 l2)
  (if (null? l1)
      nil
      (proc (car l1)
            (car l2)
            (foldr-two proc nil (cdr l1) (cdr l2)))))

(define (foldr proc nil . lsts)
  (case (length lsts)
    ((1) (foldr-one proc nil (car lsts)))
    ((2) (foldr-two proc nil (car lsts) (cadr lsts)))
    (else (error "not implemented"))))

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

(define (foldl-one proc nil lst)
  (if (null? lst)
      nil
      (foldl proc
             (proc (car lst) nil)
             (cdr lst))))

(define (foldl-two proc nil l1 l2)
  (if (null? l1)
      nil
      (foldl-two proc
                 (proc (car l1) (car l2) nil)
                 (cdr l1)
                 (cdr l2))))

(define (foldl proc nil . lsts)
  (case (length lsts)
    ((1) (foldl-one proc nil (car lsts)))
    ((2) (foldl-two proc nil (car lsts) (cadr lsts)))
    (else (error "not implemented"))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define call/cc call-with-current-continuation)

(define (identity x) x)

(define (intersperse el lst)
  (cons (car lst)
        (foldr (lambda (x xs)
                 (cons el (cons x xs)))
               '()
               (cdr lst))))

(define (string-join el lst)
  (apply string-append
         (intersperse el lst)))

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

(assert
 (symbol-starts-with '%foo '%)
 (symbol-starts-with 'bar '%) => #f)

(define (uniq seq)
  (foldl (lambda (item result)
           (if (memq item result)
               result
               (cons item result)))
         '()
         seq))

(assert
 (uniq '(a b a c d b e)) => '(e d c b a))

(define (never? _) #f)
