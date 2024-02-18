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

(define fold
  (lambda (fn init xs)
    (if (null? xs)
        init
        (fold fn (fn (car xs) init) (cdr xs)))))

(define reverse
  (lambda (xs)
    (fold cons (list) xs)))
