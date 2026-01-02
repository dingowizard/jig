(library (rnrs control)
  (export when unless
          ;case-lambda
          do)
  (import (core-primitives)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand))

  (define-syntax when
     (syntax-rules ()
        ((when test body0 bodies ...) (if test (begin body0 bodies ...)))))

  (define-syntax unless
    (syntax-rules ()
      ((unless test body0 bodies ...) (if (not test) (begin body0 bodies ...)))))

  ;; ; TODO: make step optional (see spec r6rs-lib)
  (define-syntax do
     (syntax-rules ()
        ((do ((var init step) ...)
             (test expr ...)
             command ...)
         ((lambda ()
             (define loop (lambda (var ...)
                             (if test
                                 (begin
                                    (void)
                                    expr ...)
                                 (begin
                                    command ...
                                    (loop step ...)))))
             (loop init ...))))))
  )
