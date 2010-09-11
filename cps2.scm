(define heap-spine-len 8)
(define heap-rib-len 256)

(define-record-type rtd/heap
  (make-heap* spine rib idx)
  heap?
  (spine heap-spine)
  (rib heap-rib set-heap-rib!)
  (idx heap-idx set-heap-idx!))

(define (make-heap)
  (let ((spine (make-vector heap-spine-len #f)))
    (vector-set! spine 0 (make-vector heap-rib-len #f))
    (make-heap* spine 0 0)))

(define (heap-allocate heap)
  (let ((spine (heap-spine heap))
        (rib (heap-rib heap))
        (idx (heap-idx heap)))
    (if (= idx heap-rib-len)
        (if (= rib heap-spine-len)
            (error "out of memory")
            (begin
              (vector-set! spine (+ rib 1) (make-vector heap-rib-len))
              (set-heap-rib! (+ rib 1))
              (heap-allocate heap)))
        (begin
          (set-heap-idx! heap (+ idx 1))
          (cons rib idx)))))

(define slot-rib car)
(define slot-idx cdr)

(define-record-type rtd/module
  (make-module* name heap export import)
  module?
  (name module-name)
  (heap module-heap)
  (import module-import)
  (export module-export set-module-export!))

(define (make-module name)
  (make-module* name (make-heap) #f #f))

(define current-module (make-fluid #f))

(define (ref module rib idx)
  (vector-ref
   (vector-ref (module-heap module) rib)
   idx))

(define (gensym* module)
  (let* ((heap (module-heap module))
         (slot (heap-allocate heap)))
    `(ref ,module ,(slot-rib slot) ,(slot-idx slot))))

(define (gensym)
  (gensym* (fluid current-module)))

(define (cps-begin . body)
  (let ((value (gensym)))
   `(lambda (cont)
      ,(foldr (lambda (head tail)
                `(,head (lambda (,value)
                          ,tail)))
              `(cont v)
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
