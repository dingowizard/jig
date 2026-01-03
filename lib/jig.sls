(library (jig)
  (export ; (rnrs base)
          length not
          list-tail list-ref caar cadr cdar cddr caddr cdddr positive? negative? abs reverse for-each
          memv odd? even? let or let* letrec cond case
          cons car cdr append pair? number? procedure? list? null? zero? call/cc + apply expand > < - * = eqv?
          values call-with-values dynamic-wind symbol? vector vector-ref vector? vector-length
          and quasiquote syntax-rules

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
          ; (core-primitives)
          datum->syntax syntax->datum syntax->list syntax-e
          displayln display newline
          ; (jig prelude)
          map all any void error list
          ; (jig)
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
             (value (converter init)))
        (lambda args
          (if (null? args)
              value
              (let ((len (length args)))
                (cond ((= len 1)
                       (set! value (converter (car args))))
                      ((= len 2)
                       (set! value ((cadr args) (car args))))
                      (#t 'error))))))))

  (define-syntax parameterize
     (syntax-rules ()
        ((parameterize ((p0 v0) (p v) ...) body0 body ...)
         ((lambda olds
             (dynamic-wind
                (lambda () (p0 v0) (p v) ...)
                (lambda () body0 body ...)
                (lambda () (for-each (lambda (pr old) (pr old (lambda (x) x))) (list p0 p ...) olds)))) (p0) (p) ...))))

  (define abort)



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
   (set! abort (lambda xs (displayln (car xs)) (k (void))))))

)
