(define-structure llc llc-interface
  (open scheme
        c-system-function
        os-strings
        bitwise
        srfi-13
        srfi-23
        srfi-28
        extended-ports)
  (files llvm llc))