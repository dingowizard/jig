(library (jig)
  (export ; (rnrs base)
          length not
          map list error
          list-tail list-ref caar cadr cdar cddr caddr cdddr positive? negative? abs reverse for-each
          memv odd? even? let or let* letrec cond case
          cons car cdr append pair? number? char? string? procedure? list? null? zero? call/cc apply expand > < max min + * - / = eqv?
          boolean=?
          values call-with-values dynamic-wind symbol? vector vector-ref vector? vector-length
          and quasiquote syntax-rules
          define define-syntax begin lambda if set! quote quasiquote

          ; TODO: which library?
          raise raise-continuable with-exception-handler
          ; (rnrs records procedural)
          make-record-type-descriptor record-type-descriptor? make-record-constructor-descriptor
          record-predicate record-accessor record-constructor
          ; (rnrs records inspection)
          record?
          ; (rnrs lists)
          find exists for-all filter partition fold-left fold-right memv remv assv cons*
          ; (rnrs control)
          when unless do
          ; (rnrs conditions) .. should really be (rnrs conditions (6)) whenever we get version numbers going
          condition? condition make-message-condition
          ; (core-primitives)
          datum->syntax syntax->datum syntax->list syntax-e
          displayln display newline
          ; (jig prelude)
          all any void
          ; (jig)
          template
          compose make-parameter parameterize record-constructor-descriptor?)
  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand)
          (for (rnrs base) run)
          (for (rnrs base) expand)
          (for (rnrs control) run)
          (for (rnrs control) expand)
          (for (rnrs records procedural) run)
          (for (rnrs records procedural) expand)
          (for (rnrs records inspection) run)
          (for (rnrs records inspection) expand)
          (for (rnrs lists) run)
          (for (rnrs lists) expand))

  (define compose2
      (lambda (f1 f2)
        (lambda (x)
          (f2 (f1 x)))))

  (define compose
      (lambda xs
        (fold-left compose2 (lambda (x) x) xs)))


  (define make-parameter
    (lambda (init . o)
      (let* ((converter (if (not (null? o)) (car o) (lambda (x) x)))
             (value (converter init))
             (slot (new-dyn-env-slot value)))
        (lambda args
          (if (null? args)
              (get-dyn-env-value slot)
              (let ((len (length args)))
                (cond ((= len 1)
                       (set-dyn-env-slot! slot (converter (car args))))
                      ((= len 2)
                       (set-dyn-env-slot! slot ((cadr args) (car args))))
                      (error 'parameter "expected 0, 1 or 2 arguments but received more"))))))))

  (define-syntax parameterize
     (syntax-rules ()
        ((parameterize ((p0 v0) (p v) ...) body0 body ...)
         ((lambda olds
             (dynamic-wind
                (lambda () (p0 v0) (p v) ...)
                (lambda () body0 body ...)
                (lambda () (for-each (lambda (pr old) (pr old (lambda (x) x))) (list p0 p ...) olds)))) (p0) (p) ...))))

  (define abort)

  ; TODO: move these to (rnrs conditions)
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



 (call/cc
  (lambda (k)
    (define *current-exception-handlers*
      (list (lambda (condition)
              (display "unhandled exception ")
              (display condition)
              (newline)
              (k (void)))))
    (define with-exception-handlers
        (lambda (new-handlers thunk)
            (let ((previous-handlers *current-exception-handlers*))
             (dynamic-wind
                 (lambda ()
                  (set! *current-exception-handlers* new-handlers))
                 thunk
                 (lambda ()
                  (set! *current-exception-handlers* previous-handlers))))))
    (set! with-exception-handler
        (lambda (handler thunk)
            (with-exception-handlers (cons handler *current-exception-handlers*)
                                    thunk)))
    (set! raise
        (lambda (obj)
            (let ((handlers *current-exception-handlers*))
             (with-exception-handlers (cdr handlers)
                 (lambda ()
                  ((car handlers) obj)
                  (abort "user-defined handler returned on non-continuable exception"
                          (car handlers)
                          obj))))))
    (set! raise-continuable
        (lambda (obj)
            (let ((handlers *current-exception-handlers*))
             (with-exception-handlers (cdr handlers)
                 (lambda ()
                  ((car handlers) obj))))))
    (set! abort (lambda xs (displayln (car xs)) (k (void)))))))
