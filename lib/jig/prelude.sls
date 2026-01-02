(library (jig prelude)
  (export length not error car cdr caar cadr cdar cddr caddr cdddr fold-left fold-right map all any void error list list? zero? +)
  (import (core-primitives))

  (define car
    (lambda (x)
      (if (pair? x)
          (unchecked-car x)
          (error 'car "expected its argument to be a pair")))) ; TODO: irritants
                                        ;
  (define cdr
    (lambda (x)
      (if (pair? x)
          (unchecked-cdr x)
          (error 'cdr "expected its argument to be a pair")))) ; TODO: irritants

  (define zero?
    (lambda (x)
      (if (number? x)
          (= x 0)
          (error 'zero? "expected its argument to be a number"))))

  (define list?
    (lambda (x)
      (if (null? x)
          #t
          (if (pair? x)
              (list? (cdr x))
              #f)))) ; TODO: better to have this builtin?

  ; TODO: problem redefining call/cc like this
  ;; (define call/cc
  ;;   (lambda (x)
  ;;     (if (procedure? x)
  ;;         (unchecked-call/cc x)
  ;;         (error 'call/cc "expected its argument to be a procedure"))))

  (define length
      (lambda (l)
        (define loop
          (lambda (acc l)
            (if (null? l)
                acc
                (loop (+ 1 acc) (cdr l)))))
        (loop 0 l)))

  (define not (lambda (x) (if x #f #t)))


  (define fold-right
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fn (car xs) (fold-right fn acc (cdr xs))))))

  (define +
    (lambda xs
      (fold-right (lambda (a b) (if (and (number? a) (number? b)) (unchecked-bin-op-+ a b) (error '+ "expected all arguments to be numbers"))) 0 xs)))

  (define fold-left
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fold-left fn (fn (car xs) acc) (cdr xs)))))

  ;; (define map-1
  ;;   (lambda (fn xs)
  ;;     (if (null? xs)
  ;;         '()
  ;;         (cons (fn (car xs)) (map-1 fn (cdr xs))))))

  (define map
    (lambda (fn xs . rest)
      ((lambda (ls)
          (if (not (apply = (fold-right (lambda (x acc) (cons (length x) acc)) '() ls)))
              (error "map: lists must be same length" ls))
        (if (any null? ls)
            '()
            (cons (apply fn (fold-right (lambda (x acc) (cons (car x) acc)) '() ls))
                  (apply map (cons fn (fold-right (lambda (x acc) (cons (cdr x) acc)) '() ls)))))) (cons xs rest))))

  (define all
     (lambda (pred xs)
        (if (null? xs)
            #t
            (if (pred (car xs)) (all pred (cdr xs)) #f))))

  (define any
     (lambda (pred xs)
        (call/cc (lambda (return)
                    (if (null? xs)
                        #f
                        (if (pred (car xs))
                            (return #t)
                            (any pred (cdr xs))))))))

  (define void (lambda () (if #f #f)))

  ;; (define-syntax or2
  ;;   (lambda (stx)
  ;;     (datum->syntax stx `((lambda (x) (if x x ,(car (cdr (cdr (syntax->list stx)))))) ,(car (cdr (syntax->list stx)))))))

  ;; ; ; TODO: shouldn't this call raise or raise continuable?
  ;; (call/cc
  ;;  (lambda (k)
  ;;    (set! error
  ;;          (lambda (msg . xs)
  ;;            (display msg)
  ;;            (newline)
  ;;            (when (not (null? xs))
  ;;               (display "    in:")
  ;;               (display (car xs))
  ;;               (newline))
  ;;            (k (void))))))

  (define list (lambda xs xs))

  (call/cc
   (lambda (k)
     (set! error (lambda args (k 'error)))
     (void)))
  )
