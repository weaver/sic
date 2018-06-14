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
(define FIXNUM-MASK #x03)
(define FIXNUM-TAG #x00)
(define CHAR-SHIFT 8)
(define CHAR-TAG #x0F)
(define CHAR-MASK #xFF)
(define BOOL-SHIFT 8)
(define BOOL-TAG #x3F)
(define BOOL-MASK #xFF)
(define EMPTY-LIST #x2F)

(define (immediate? expr)
  (or (integer? expr)
      (char? expr)
      (boolean? expr)
      (null? expr)))

(define (immediate-repr expr)
  (cond
   ((integer? expr)
    (arithmetic-shift expr FIXNUM-SHIFT))
   ((char? expr)
    (bitwise-ior
     (arithmetic-shift (char->integer expr) CHAR-SHIFT)
     CHAR-TAG))
   ((boolean? expr)
    (bitwise-ior
     (arithmetic-shift (if expr 1 0) BOOL-SHIFT)
     BOOL-TAG))
   ((null? expr)
    EMPTY-LIST)
   (else
    (raise (compilation-error 'no-immediate-expression expr)))))

(define (emit-immediate expr)
  (emit "movl $" (immediate-repr expr) ", %eax"))

(define (unary-primcall? expr)
  (and (pair? expr)
       (memq (primcall-op expr)
	     '(add1 sub1 integer->char char->integer null? zero? not integer? boolean? char?))))

(define primcall-op car)
(define primcall-operand1 cadr)
(define (primcall-operand2 expr)
  (car (cddr expr)))

(define (emit-unary-primcall expr si)

  (define (compare-to literal)
    (emit "cmpl $" literal ", %eax")
    (emit "movl $0, %eax")
    (emit "sete %al"))

  (define (test-mask mask tag)
    (emit "andb $" mask ", %al")
    (emit "cmpb $" tag ", %al")
    (emit "movl $0, %eax")
    (emit "sete %al"))

  (define (shift-left-and-tag shift tag)
    (emit "shll $" shift ", %eax")
    (emit "orl $" tag ", %eax"))

  (define (predicate op . args)
    (apply op args)
    (shift-left-and-tag BOOL-SHIFT BOOL-TAG))

  (emit-expr (primcall-operand1 expr) si)
  (case (primcall-op expr)
    ((add1)
     (emit "addl $" (immediate-repr 1) ", %eax"))
    ((sub1)
     (emit "subl $" (immediate-repr 1) ", %eax"))
    ((integer->char)
     (shift-left-and-tag (- CHAR-SHIFT FIXNUM-SHIFT) CHAR-TAG))
    ((char->integer)
     (emit "shrl $" (- CHAR-SHIFT FIXNUM-SHIFT) ", %eax"))
    ((null?)
     (predicate compare-to EMPTY-LIST))
    ((zero?)
     (predicate compare-to 0))
    ((integer?)
     (predicate test-mask FIXNUM-MASK FIXNUM-TAG))
    ((char?)
     (predicate test-mask CHAR-MASK CHAR-TAG))
    ((boolean?)
     (predicate test-mask BOOL-MASK BOOL-TAG))
    ((not)
     ;; A false value will be zero shifted by BOOL-SHIFT and then
     ;; tagged (equal to BOOL-TAG)
     (predicate compare-to BOOL-TAG))
    (else
     (raise (compilation-error 'unrecognized-unary-primcall expr)))))

(define (binary-primcall? expr)
  (and (pair? expr)
       (memq (primcall-op expr)
	     '(+ - * = < char=?))))

(define WORDSIZE 4)

(define (init-stack-index)
  (* WORDSIZE -1))

(define (next-stack-index si)
  (- si WORDSIZE))

(define (emit-binary-primcall expr si)
  (emit-expr (primcall-operand2 expr) si)
  (emit "movl %eax, " si "(%rsp)")
  (emit-expr
   (primcall-operand1 expr)
   (next-stack-index si))
  (case (primcall-op expr)
    ((+)
     (emit "addl " si "(%rsp), %eax"))
    (else
     (raise (compilation-error 'unrecognized-binary-primcall expr)))))

(define (emit-expr expr si)
  (cond
   ((immediate? expr)
    (emit-immediate expr))
   ((unary-primcall? expr)
    (emit-unary-primcall expr si))
   ((binary-primcall? expr)
    (emit-binary-primcall expr si))
   (else
    (raise (compilation-error 'unrecognized-expr expr)))))

(define (compile-program-clang expr)
  (emit-all
   "	.section	__TEXT,__text,regular,pure_instructions"
   "	.globl	_scheme_entry"
   "	.p2align	4, 0x90"
   "_scheme_entry:"
   "	.cfi_startproc"
   "	.cfi_def_cfa_offset 16")

  (emit-expr expr (init-stack-index))
  (emit "retq")

  (emit-all
   "	.cfi_endproc"))

(define (compile-program-gcc expr)
  (emit-all
   "	.file	\"test.c\""
   "	.text"
   "	.globl	scheme_entry"
   "	.type	scheme_entry, @function"
   "scheme_entry:"
   ".LFB0:"
   "	.cfi_startproc")

  (emit-expr expr (init-stack-index))

  (emit-all
    "	ret"
    "	.cfi_endproc"
    ".LFE0:"
    "	.size	scheme_entry, .-scheme_entry"))

(define compile-program compile-program-clang)


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
       (print "✗ FAIL  " source ": expected {" expect "}, got {" result "}")
       #f)
     (else
      (print "✔ OK    " source " => " expect)))))

(test-section
 "3.1 Integers"
 (test-case 42 "42")
 (test-case -1 "-1"))

(test-section
 "3.2 Intermediate Constants"
 (test-case #\c "#\\c")
 (test-case #\" "#\\\"")
 (test-case #t "#t")
 (test-case #f "#f")
 (test-case (list) "'()"))

(test-section
 "3.3 Unary Operations"
 (test-case '(add1 42) "43")
 (test-case '(sub1 42) "41")

 (test-case '(integer->char 42) "#\\*")
 (test-case '(char->integer #\*) "42")

 (test-case '(not #f) "#t")
 (test-case '(not #t) "#f")
 (test-case '(not 42) "#f")
 (test-case '(not 0) "#f")

 (test-case '(boolean? #t) "#t")
 (test-case '(boolean? #f) "#t")
 (test-case '(boolean? 42) "#f")

 (test-case '(null? 42) "#f")
 (test-case '(null? ()) "#t")

 (test-case '(integer? 42) "#t")
 (test-case '(integer? #t) "#f")

 (test-case '(zero? 0) "#t")
 (test-case '(zero? 42) "#f")
 (test-case '(zero? #f) "#f")
 (test-case '(zero? ()) "#f")

 (test-case '(char? #\*) "#t")
 (test-case '(char? 42) "#f"))

(test-section
 "3.4 Binary Operations"
 (test-case '(+ 2 3) "5")
 (test-case '(+ 3 (add1 4)) "8")
 (test-case '(add1 4) "5"))
