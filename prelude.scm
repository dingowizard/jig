(define length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (cdr l))))))

(define list (lambda l l))

(define list-tail
  (lambda (x k)
    (if (= k 0)
        x
        (list-tail (cdr x) (- k 1)))))

(define list-ref
  (lambda (xs k)
    (if (= k 0)
        (car xs)
        (list-ref (cdr xs) (- k 1)))))

(define zero? (lambda (x) (= x 0)))

(define not (lambda (x) (if x #f #t)))

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

(define caar (lambda (p) (car (car p))))

(define cadr (lambda (p) (car (cdr p))))

(define cdar (lambda (p) (cdr (car p))))

(define cddr (lambda (p) (cdr (cdr p))))

(define caddr (lambda (p) (car (cdr (cdr p)))))

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
    (let ((stx-list (syntax->list stx)))
      (if (= 1 (length stx-list))
          (datum->syntax stx (quote #f))
          (datum->syntax
           stx
           (list 'let
                 (list (list 'x (car (cdr stx-list))))
                 (list 'if 'x 'x (append (list 'or) (cdr (cdr stx-list))))))))))
