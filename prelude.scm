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

(define map
  (lambda (fn xs)
    (if (null? xs)
        '()
        (cons (fn (car xs)) (map fn (cdr xs))))))

(define append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2)))))

(define-syntax let
  (lambda (stx)
    (datum->syntax
     stx
    (cons
      (append (list 'lambda
                    (map (lambda (b) (car (syntax->list b))) (syntax->list (car (cdr (syntax->list stx))))))
              (cdr (cdr (syntax->list stx))))
      (map (lambda (b) (car (cdr (syntax->list b)))) (syntax->list (car (cdr (syntax->list stx)))))))))

(define-syntax or
  (lambda (stx)
    (datum->syntax
     stx
     (list
      (list 'lambda (list 'tmp) (list 'if 'tmp 'tmp (car (cdr (cdr (syntax->list stx))))))
      (car (cdr (syntax->list stx)))))))
