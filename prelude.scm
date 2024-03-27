(define list (lambda xs xs))

(define length
    (lambda (l)
      (define loop
        (lambda (acc l)
          (if (null? l)
              acc
              (loop (+ 1 acc) (cdr l)))))
      (loop 0 l)))

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

(define-syntax begin
  (lambda (stx)
    (datum->syntax
     stx
     (list
      (append (list 'lambda (list))
              (cdr (syntax->list stx)))))))

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

(define-syntax let*
  (lambda (stx)
    (let ((syntax-list (syntax->list stx)))
      (let ((bindings (syntax->list (cadr syntax-list))))
        (datum->syntax
         stx
         (let ((len (length bindings))
               (body (cddr syntax-list)))
          (if (= len 0)
              `((lambda () ,@body))
              (if (= len 1)
                  `(let (,(car bindings)) ,@body)
                  `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))))))

(define-syntax cond
  (lambda (stx)
    (let ((clauses (cdr (syntax->list stx))))
      (let ((clause1 (syntax->list (car clauses))))
        (datum->syntax
         stx
         (if (= (length clauses) 1)
             `(if ,(car clause1)
                  ,(cadr clause1)
                  #f)
             `(if ,(car clause1)
                  ,(cadr clause1)
                  (cond ,@(cdr clauses)))))))))
