;; Main program
;; ,build main llc.img
(define (main args)
  (if (not (= (length args) 1))
      (usage)
      (apply file->executable (map os-string->string args)))
  (return 0))

(define (usage)
  (display "usage: llc <filename>")
  (newline))

(define (return value)
  value)


;;; Testing

;; Use this test harness compile an INPUT form and compare the output
;; of the program with an expected result.
(define (test-case input expect)
  (emit->executable input "test")
  (let ((actual (read-output-file "./test > test.out" "test.out")))
    (or (string=? expect actual)
        (error 'test-failed `(input: ,input expected: ,expect actual: ,actual)))))


;;; Compile File

;; Compile a single file full of llvm expressions into an executable
(define (file->executable script)
  (let ((name (executable-name script)))
    (emit->executable (read-all-from-file script) name)
    (display (format "OK, compiled ~a -> ~a" script name))
    (newline)))

;; Make an executable name by stripping off the extension of the
;; source file.
(define (executable-name source)
  (let ((name (remove-ext source)))
    (if (string=? source name)
        (format "~a.out" name)
        name)))

(define (read-all-from-file file)
  ;; (with-input-from-file file read-all)
  (with-input-from-file file read))

(define (read-all)
  (let slurp ((input '(begin)))
    (let ((expr (read)))
      (if (eof-object? expr)
          input
          (slurp (append input expr))))))


;;; Emitter

;; Emit a single instruction
(define (emit . template)
  (display "  ")
  (display (apply format template))
  (newline))

(define (emit->executable input name)
  (input->scheme-entry input (format "~a.ll" name))
  (llvm->executable name))

(define (input->scheme-entry input file)
  (with-output-to-file file
    (lambda ()
      (display "define i64 @scheme_entry() nounwind {\n")
      (compile-program input)
      (display "}\n"))))


;;; External compilers

;; Compile an LLVM assembler file with the run-time to create an
;; executable binary.
(define (llvm->executable name)
  (must "llc ~a.ll && gcc run-time.c ~a.s -o ~a" name name name))

;; (display (c->x86 "goal"))
(define (c->x86 name)
  (read-output-file
   (format "gcc -O3 --omit-frame-pointer -S ~a.c" name)
   (format "~a.s" name)))

;; (display (c->llvm "run-time"))
(define (c->llvm name)
  (read-output-file
   (format "llvm-gcc -S -emit-llvm ~a.c" name)
   (format "~a.s" name)))


;;; Aux

;; Execute COMMAND, which produced an output FILE.  Read the contents
;; of FILE into a string.
(define (read-output-file command file)
  (must command)
  (file->string file))

;; Raise an ERROR if COMMAND doesn't exit successfully.
(define (must template . args)
  (let* ((command (apply format template args))
         (code (system command)))
    (if (not (= code 0))
        (error 'unexpected-exit-code code command))))

;; Read the contents of FILE into a string.
(define (file->string file)
  (call-with-input-file file port->string))

;; Read the contents of PORT into a string.
(define (port->string port)
  (do ((buffer (make-string-output-port))
       (datum (read-char port) (read-char port)))
      ((eof-object? datum) (string-output-port-output buffer))
    (write-char datum buffer)))

;; Remove the extension from a file name
(define (remove-ext name)
  (let ((dot (string-index-right name #\.)))
    (if dot
        (substring name 0 dot)
        name)))