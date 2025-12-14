(library (rnrs lists)
  (export all any fold-right length list map)
  (import (core-primitives))

  ;; TODO: not rnrs I don't think
  ;; NOTE: syntax-case uses all
    (define all
       (lambda (pred xs)
          (if (null? xs)
              #t
              (if (pred (car xs)) (all pred (cdr xs)) #f))))

  ;; TODO: any is a SRFI-1 procedure, I think.
  ;; NOTE: map uses any. maybe don't export it
    (define any
       (lambda (pred xs)
          (call/cc (lambda (return)
                      (if (null? xs)
                          #f
                          (if (pred (car xs))
                              (return #t)
                              (any pred (cdr xs))))))))

    (define fold-right
      (lambda (fn acc xs)
        (if (null? xs)
            acc
            (fn (car xs) (fold-right fn acc (cdr xs))))))

    (define length
      (lambda (l)
        (define loop
          (lambda (acc l)
            (if (null? l)
                acc
                (loop (+ 1 acc) (cdr l)))))
        (loop 0 l)))

    (define list (lambda xs xs))

    (define map
      (lambda (fn xs . rest)
        ((lambda (ls)
            (if (not (apply = (fold-right (lambda (x acc) (cons (length x) acc)) '() ls)))
                (error "map: lists must be same length" ls))
          (if (any null? ls)
              '()
              (cons (apply fn (fold-right (lambda (x acc) (cons (car x) acc)) '() ls))
                    (apply map (cons fn (fold-right (lambda (x acc) (cons (cdr x) acc)) '() ls)))))) (cons xs rest))))

  )
