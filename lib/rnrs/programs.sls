(library (rnrs programs)
  (export exit)
  (import (core-primitives)
          (jig prelude))

  (define exit
    (lambda args
      (if (zero? (length args))
          (unsafe-exit 0)
          (if (= 1 (length args))
              (if (integer? (car args))
                  (unsafe-exit (car args))
                  (error 'exit "expected argument to be a positive integer" (car args)))
              (error 'exit "expected one optional argument" args))))))
