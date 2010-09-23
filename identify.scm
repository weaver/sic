;;;; Syntax Closures
(define-record-type rtd/syntax-closure
  (stx-make* name type gensym environment tail)
  stx?
  (name stx-name)
  (type stx-type)
  (gensym stx-gensym set-stx-gensym!)
  (environment stx-envr)
  (tail stx-tail))

(define (stx-make name type tail)
  (stx-make* name type 0 tail))

(define (stx-null) stx-null)
(define (stx-null? x) (eq? x stx-null))

(define (stx-foldr proc nil stx)
  (foldr* proc nil stx identity stx-tail stx-null?))

(define (stx-map proc stx)
  (syntax-foldr (lambda (x xs) (cons (proc x) xs)) '() stx))

(define (stx-for-each proc stx)
  (syntax-foldr (lambda (x xs) (proc x)) #f stx))

(define (stx-name stx)
  (stx-map (lambda (x) (slot x 'name)) stx))

(define (stx-llvm stx) (string-join ":" (stx-name stx)))

(define (stx-ref s key)
  (table-ref (syntax-table s) key))

(define (stx-set! s key val)
  (table-set! (syntax-table s) key val))

(define (stx++! stx)
  (let ((count (+ (stx-gensym stx) 1)))
    (set-stx-gensym! stx count)
    count))

(define (gensym* prefix stx)
  (string->symbol
   (string-append
    (stx-llvm stx)
    ":"
    prefix
    ":"
    (number->string (stx++! stx)))))

(define (vsym s) (gensym* "v" s))
(define (ksym s) (gensym* "k" s))

(define (with-syntax) with-syntax)
(define (with-syntax? x) (eq? with-syntax))

;;;; Expression Types

(define (variable? s e)
  (call/cc
   (lambda (k)
     (stx-for-each
      (lambda (s)
        (let ((val (stx-ref s e)))
          (k val)))
      s)
     #f)))

(define (constant? e)
  (or (number? e) (character? e) (string? e)))

(define (primitive? e)
  (procedure? e))

(define (atom? e)
  (not (pair? e)))

(define (lambda? e)
  #f)




(define stx-sic
  '((type . module)
    (name . (sic))
    (tail . ,stx-null)
    (envr .
          ((lambda . ,(lambda (stx expr)
                        (let ((syms (cadr expr))
                              (body (identify-begun (cdddr expr))))
                          (with-syntax
                           stx
                           `(lambda (,@args) ,@body))
)))
           )

          )
    )
  )
