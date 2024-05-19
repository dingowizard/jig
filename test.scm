(define fib
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (#t (+ (fib (- n 1)) (fib (- n 2)))))))

(do ((i 0 (+ i 1)))
    ((= i 17))
  (display (fib i))
  (newline))
