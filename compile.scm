(import (scheme base)
	(scheme write)
	(scheme file)
	(chibi process)
	(srfi 33))

(define (print . args)
  (for-each display args)
  (newline))


;; Compiler

(define emit print)

(define (emit-all . text)
  (for-each emit text))

(define (compilation-error message . args)
  (list 'compilation-error message args))

(define FIXNUM-SHIFT 2)
(define CHAR-SHIFT 8)
(define CHAR-TAG #x0F)

(define (immediate-repr source)
  (cond
   ((integer? source)
    (arithmetic-shift source FIXNUM-SHIFT))
   ((char? source)
    (bitwise-ior
     (arithmetic-shift (char->integer source) CHAR-SHIFT)
     CHAR-TAG))
   (else
    (raise (compilation-error "no immediate-repr" source)))))

(define (compile-program source)
  (emit-all
   "	.section	__TEXT,__text,regular,pure_instructions"
   "	.globl	_scheme_entry"
   "	.p2align	4, 0x90"
   "_scheme_entry:"
   "	.cfi_startproc")

  (emit "movl $" (immediate-repr source) ", %eax")
  (emit "retq")

  (emit-all
   "	.cfi_endproc"))


;; Tests

(define (test-section name . cases)
  (print "Testing " name)
  (for-each evaluate-test cases))

(define test-case cons)
(define test-case-source car)
(define test-case-expect cdr)

(define (compile-test! source)
  (with-output-to-file "test.s"
    (lambda ()
      (compile-program source))))

(define (run-test! source)
  (compile-test! source)
  (and (system? '("/bin/sh" "-c" "make --silent build-test"))
       (process->string "./test")))

(define (evaluate-test test)
  (let* ((source (test-case-source test))
	 (expect (test-case-expect test))
	 (result (run-test! source)))
    (cond
     ((eq? result #f)
       (print "ERROR " source)
       #f)
     ((not (string=? result expect))
       (print "✗ FAIL " source ": expected {" expect "}, got {" result "}")
       #f)
     (else
      (print "✔ OK " source)))))

(test-section
 "3.1 Integers"
 (test-case 42 "42")
 (test-case -1 "-1"))

(test-section
 "3.2 Intermediate Constants"
 (test-case #\c "#\\c")
 (test-case #\" "#\\\""))
