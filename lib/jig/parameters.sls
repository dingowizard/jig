(library (jig parameters)
  (export make-parameter parameterize)
  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand)
          (for (rnrs base) run))

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
                (lambda () (for-each (lambda (pr old) (pr old (lambda (x) x))) (list p0 p ...) olds)))) (p0) (p) ...)))))
  


  
