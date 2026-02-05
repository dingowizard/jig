(library (rnrs conditions)
  (export condition? make-message-condition condition condition-message message-condition?)
  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (rnrs lists)run))

  (define make-message-condition
    (lambda (msg)
      (if (string? msg)
          (make-message-condition-unsafe msg)
          (error 'make-message-condition "expected a string argument." msg))))
  
  (define condition
    (lambda cs
      (if (all condition? cs)
          (apply condition-unsafe cs)
          (error 'condition "expected all arguments to be conditions" (find (lambda (x) (not (condition? x))) cs)))))

  (define condition-message
    (lambda (c)
      (if (condition? c)
          (condition-message-unsafe c)
          (error 'condition-message "expected a condition argument." c)))))
