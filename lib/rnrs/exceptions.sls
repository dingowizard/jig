(library (rnrs exceptions)
  (export with-exception-handler raise raise-continuable)
  (import (jig parameters)
          (core-primitives)
          (jig prelude)
          (rnrs base))


  (define *current-exception-handlers*
    (lambda args
        (if (null? args)
            (get-dyn-env-value 0)
            (let ((len (length args)))
              (cond ((= len 1)
                     (set-dyn-env-slot! 0 (car args)))
                    ((= len 2)
                     (set-dyn-env-slot! 0 ((cadr args) (car args))))
                    (error 'parameter "expected 0, 1 or 2 arguments but received more"))))))
  (define with-exception-handler)
  (define raise)
  (define raise-continuable)

  (call/cc
    (lambda (k)

      (define abort (lambda xs (displayln (car xs)) (k (void))))
      (set! with-exception-handler
            (lambda (handler thunk)
              ((lambda (old)
                 (dynamic-wind
                    (lambda () (*current-exception-handlers* (cons handler old)))
                    (lambda () (thunk))
                    (lambda () (*current-exception-handlers* old)))) (*current-exception-handlers*))))
              
      (set! raise
          (lambda (obj)
              (let ((handler (car (*current-exception-handlers*))))
               (parameterize ((*current-exception-handlers* (cdr (*current-exception-handlers*))))
                    (handler obj)
                    (abort "user-defined handler returned on non-continuable exception"
                            handler
                            obj)))))
      (set! raise-continuable
          (lambda (obj)
              (let ((handler (car (*current-exception-handlers*))))
               (parameterize ((*current-exception-handlers* (cdr (*current-exception-handlers*))))
                    (handler obj))))))))


  
