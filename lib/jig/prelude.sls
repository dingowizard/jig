(library (jig prelude)
  (export length not caar cadr cdar cddr caddr cdddr fold-right map all any void error list)
  (import (core-primitives))

  (define length
      (lambda (l)
        (define loop
          (lambda (acc l)
            (if (null? l)
                acc
                (loop (+ 1 acc) (cdr l)))))
        (loop 0 l)))

  (define not (lambda (x) (if x #f #t)))

  (define caar (lambda (p) (car (car p))))

  (define cadr (lambda (p) (car (cdr p))))

  (define cdar (lambda (p) (cdr (car p))))

  (define cddr (lambda (p) (cdr (cdr p))))

  (define caddr (lambda (p) (car (cdr (cdr p)))))

  (define cdddr (lambda (p) (cdr (cdr (cdr p)))))

  (define fold-right
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fn (car xs) (fold-right fn acc (cdr xs))))))

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
  (define error #f)

  (define-syntax or2
    (lambda (stx)
      (datum->syntax stx `((lambda (x) (if x x ,(car (cdr (cdr (syntax->list stx)))))) ,(car (cdr (syntax->list stx)))))))

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
