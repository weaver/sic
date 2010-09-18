;;;; Step 1 -- Compile Integers

(define (compile-program form)
  (emit "ret i64" form))

(test-case 42 "42")

,exit
