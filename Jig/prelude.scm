(define length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (cdr l))))))

(define list
  (lambda l l))

(define not
  (lambda (x)
    (if x #f #t)))

(define list?
  (lambda (x)
    (if (null? x)
        #t
        (if (pair? x)
            (list? (cdr x))
            #f))))

(define-syntax or
  (lambda (stx)
    (datum->syntax
     stx
     (list
      (list 'lambda (list 'tmp) (list 'if 'tmp 'tmp (car (cdr (cdr (syntax->list stx))))))
      (car (cdr (syntax->list stx)))))))
