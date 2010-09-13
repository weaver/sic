(define-record-type rtd/module
  (make-module* name heap import export gensym)
  module?
  (name module-name)
  (heap module-heap)
  (import module-import)
  (export module-export set-module-export!)
  (gensym module-gensym set-module-gensym!))

(define (make-module name)
  (make-module* name (make-heap) #f #f 0))

;; (define current-module (make-fluid #f))

(define (gensym* module)
  (let ((gensym (module-gensym module))
        (prefix (string-append (module-name module) ":")))
    (set-module-gensym! module (+ gensym 1))
    (string->symbol
     (string-append prefix (number->string gensym)))))

;; (define (gensym)
;;   (gensym* (fluid current-module)))

(define (cps-begin . body)
  (let ((value (gensym)))
   `(lambda (cont)
      ,(foldr (lambda (head tail)
                `(,head (lambda (,value)
                          ,tail)))
              `(cont ,value)
              body))))

(define current-module (make-module "test"))
(define (gensym) (gensym* current-module))

(assert
 (cps-begin 1 2) =>
 '(lambda (cont)
    (1 (lambda (v)
         (2 (lambda (v)
              (cont v))))))
 )

(define (cps-arguments . body)
  (let* ((symbols (map (lambda (x) (gensym)) body))
         (cur symbols)
         (sym (lambda ()
                (begin1 (car cur)
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
