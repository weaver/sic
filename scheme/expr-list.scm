;;;; This is the original set of utilities for the cps transform. The
;;;; idea is to replace this interface with an implementation that
;;;; supports modules. The utilities are Weaver's, the module stuff is
;;;; Lang's.

(define (make-gensym default seed)
  (lambda prefix
    (set! seed (+ seed 1))
    (string->symbol
     (string-append
      (symbol->string (if (null? prefix) default (car prefix)))
      "."
      (number->string seed)))))

(define gensym (make-gensym 'g 0))

(let ()
  (assert
   (gensym) => 'g.1
   (gensym) => 'g.2
   (gensym 'k) => 'k.3))

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

(let ()
  (assert
   (symbol-starts-with '%foo '%)
   (symbol-starts-with 'bar '%) => #f))

(define (foldr kons seed data)
  (if (null? data)
      seed
      (kons (car data)
            (foldr kons seed (cdr data)))))

(define (foldl kons seed data)
  (if (null? data)
      seed
      (foldl kons
             (kons (car data) seed)
             (cdr data))))

(define (uniq seq)
  (foldl (lambda (item result)
           (if (memq item result)
               result
               (cons item result)))
         '()
         seq))

(let ()
  (assert
   (uniq '(a b a c d b e)) => '(e d c b a)))

(define (never? _) #f)

;;;; Grammar
(define identifier? variable?)

;; Stub
(define (primitive? expr)
  (and (identifier? expr)
       (or (symbol-starts-with expr '%)
           (eq? expr 'if)
           (eq? expr 'set!))))

(define (atom? expr)
  (or (self-eval? expr)
      (quoted? expr)
      (identifier? expr)))

(define application? pair?)
