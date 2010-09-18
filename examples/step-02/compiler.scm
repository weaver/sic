;;;; Step 2 -- Immediate Values

,open bitwise

(define (compile-program input)
  (emit "ret i64" (immediate-repr input)))

(define (immediate-repr value)
  (cond ((fixnum? value)
         (shift/tag value *FIXNUM-SHIFT* *FIXNUM-TAG*))
        ((char? value)
         (shift/tag (char->integer value) *CHAR-SHIFT* *CHAR-TAG*))
        ((boolean? value)
         (if value *TRUE* *FALSE*))
        ((null? value)
         *NULL*)
        (else
         (error 'no-immediate-repr value))))

(define (shift/tag value shift tag)
  (bitwise-ior (arithmetic-shift value shift) tag))

(define fixnum? integer?)

(define (boolean->integer value)
  (if value 1 0))

(define *FIXNUM-SHIFT* #x02)
(define *FIXNUM-MASK*  #x03)
(define *FIXNUM-TAG*   #x00)

(define *CHAR-SHIFT*   #x08)
(define *CHAR-MASK*    #xff)
(define *CHAR-TAG*     #x0f)

(define *BOOL-SHIFT*   #x07)
(define *BOOL-MASK*    #x7f)
(define *BOOL-TAG*     #x1f)
(define *FALSE*        #x1f) ; (shift/tag 0 *BOOL-SHIFT* *BOOL-TAG*)
(define *TRUE*         #x9f) ; (shift/tag 1 *BOOL-SHIFT* *BOOL-TAG*)

(define *NULL*         #x2f)

(test-case 42 "42")
(test-case #\c "#\\c")
(test-case #t "#t")
(test-case #f "#f")
(test-case '() "()")

,exit
