(library (rnrs lists)
  (export find for-all exists filter partition fold-right fold-left
          ; remp remove
          remv ; remq
          ; memp member
          memv ; TODO: why can't I have a bare parenthesis on a line after commented out lines?
          ; memq
          ; assp assoc
          assv ; assq
          cons*)
          ;
  (import (core-primitives)
          (jig prelude) ; TODO: just for any?
          (for (rnrs base) run))

  (define find
    (lambda (pred lst)
      (if (null? lst)
          #f
          (let ((x (car lst)))
            (if (pred x)
                x
                (find pred (cdr lst)))))))

  (define exists any)

  (define for-all ; aka every
    (lambda (pred xs . rest)
      (let ((ls (cons xs rest)))
        (call/cc (lambda (cc)
                   (define loop
                     (lambda (rem)
                       (if (any null? rem) ; TODO: should have tested that all ls the same length
                           #t
                           (if (apply pred (map car rem))
                               (loop (map cdr rem))
                               (cc #f)))))
                   (loop ls))))))

  (define filter
     (lambda (pred xs)
        (if (null? xs)
            '()
            (if (pred (car xs))
                (cons (car xs) (filter pred (cdr xs)))
                (filter pred (cdr xs))))))

  (define partition
     (lambda (pred xs)
        (define loop
           (lambda (ts fs xs)
              (if (null? xs)
                  (values (reverse ts) (reverse fs))
                  (if (pred (car xs))
                      (loop (cons (car xs) ts) fs (cdr xs))
                      (loop ts (cons (car xs) fs) (cdr xs))))))
        (loop '() '() xs)))

  (define memv
     (lambda (x xs)
        (if (null? xs)
            #f
            (if (eqv? (car xs) x)
                xs
                (memv x (cdr xs))))))

  (define remv
    (lambda (x xs)
      (if (null? xs)
          '()
          (let ((y (car xs)))
            (if (eqv? x y)
                (remv x (cdr xs))
                (cons y (remv x (cdr xs))))))))

  (define assv
    (lambda (key alist)
      (if (null? alist)
          #f
          (if (eqv? key (caar alist))
              (car alist)
              (assv key (cdr alist))))))

  (define cons*
    (lambda (x . rest)
      (if (null? rest)
          x
          (cons x (apply cons* rest)))))

  )
