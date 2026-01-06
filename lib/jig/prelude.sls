(library (jig prelude)
  (export append apply length not error car cdr caar cadr cdar cddr caddr cdddr
          fold-left fold-right map all any void error list list? zero? + * - / =
          raise raise-continuable with-exception-handler)
  (import (core-primitives))

  (define car
    (lambda (x)
      (if (pair? x)
          (unchecked-car x)
          (error 'car "expected its argument to be a pair." x)))) ; TODO: irritants
                                        ;
  (define cdr
    (lambda (x)
      (if (pair? x)
          (unchecked-cdr x)
          (error 'cdr "expected its argument to be a pair." x)))) ; TODO: irritants

  (define apply
    (lambda (p xs)
      (if (procedure? p)
          (if (list? xs)
              (unchecked-apply p xs)
              (error 'apply "expected second argument to be a proper list." xs))
          (error 'apply "expected first arguement to be a procedure." p))))

  (define zero?
    (lambda (x)
      (if (number? x)
          (= x 0)
          (error 'zero? "expected its argument to be a number."))))

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

  (define length ; TODO: error if not list
      (lambda (l)
        (define loop
          (lambda (acc l)
            (if (null? l)
                acc
                (loop (+ 1 acc) (cdr l)))))
        (loop 0 l)))

  (define append
    (lambda ls
      (if (null? ls)
          '()
          ((lambda (first rest)
             (if (null? rest)
                 first
                 (if (null? first)
                     (apply append rest)
                     (if (list? first)
                         (cons (car first) (apply append (cons (cdr first) rest)))
                         (error 'append "expected a proper list." first)))))
           (car ls) (cdr ls)))))

  (define not (lambda (x) (if x #f #t)))


  (define fold-left
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fold-left fn (fn acc (car xs)) (cdr xs)))))

  (define fold-right
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fn (car xs) (fold-right fn acc (cdr xs))))))

  (define +
    (lambda xs
      (fold-left (lambda (a b)
                    (if (number? b)
                        (unchecked-bin-op-+ a b)
                        (error '+ "expected all arguments to be numbers." a)))
                 0
                 xs)))

  (define *
    (lambda xs
      (fold-left (lambda (a b)
                    (if (number? b)
                        (unchecked-bin-op-* a b)
                        (error '+ "expected all arguments to be numbers." a)))
                 1
                 xs)))

  (define -
    (lambda (x . rest)
      (if (number? x)
          (if (null? rest)
              (unchecked-bin-op-- 0 x)
              (fold-left (lambda (a b)
                            (if (number? b)
                                (unchecked-bin-op-- a b)
                                (error '- "expected all arguments to be numbers" a)))
                         x
                         rest))
          (error '- "expected first argument to be a number" x))))

  (define /
    (lambda (x . rest)
      (if (number? x)
          (if (null? rest)
              (if (zero? x) (error '/ "division by zero is undefined") (unchecked-bin-op-/ 1 x))
              (fold-left (lambda (a b)
                            (if (number? b)
                                (if (zero? b)
                                  (error '/ "division by zero is undefined")
                                  (unchecked-bin-op-/ a b))
                                (error '/ "expected all arguments to be numbers" a)))
                         x
                         rest))
          (error '/ "expected first argument to be a number" x))))

  ; NOTE: this fo-each is just for = below. Not exported
  (define for-each
    (lambda (fn xs)
      (if (null? xs) (void) (begin (fn (car xs)) (for-each fn (cdr xs))))))

  (define =
    (lambda (x . xs)
      (if (number? x)
          (call/cc (lambda (return)
                     (for-each (lambda (y)
                                 (if (number? y)
                                     (if (unchecked-bin-op-= x y)
                                         (void)
                                         (return #f))
                                     (error '= "expected all arguments to be numbers." y)))
                               xs)
                     #t))
          (error '= "expected first argument to be a number." x))))


  (define map
    (lambda (fn xs . rest)
      ((lambda (ls)
          (if (not (apply = (fold-right (lambda (x acc) (cons (length x) acc)) '() ls)))
              (error 'map "lists must be same length." ls))
        (if (any null? ls)
            '()
            (cons (apply fn (fold-right (lambda (x acc) (cons (car x) acc)) '() ls))
                  (apply map (cons fn (fold-right (lambda (x acc) (cons (cdr x) acc)) '() ls)))))) (cons xs rest))))

  (define all
     (lambda (pred xs)
        (if (null? xs)
            #t
            (if (pred (car xs))
                (all pred (cdr xs))
                #f))))

  (define any
     (lambda (pred xs)
        (call/cc (lambda (return)
                    (if (null? xs)
                        #f
                        (if (pred (car xs))
                            (return #t)
                            (any pred (cdr xs))))))))

  (define void (lambda () (if #f #f)))

  (define list (lambda xs xs))

  (define with-exception-handler #f)
  (define raise #f)
  (define raise-continuable #f)

  (call/cc
   (lambda (k)
     (set! error
           (lambda (who msg . irritants)
             (display "error in ")
             (display who)
             (display ": ")
             (display msg)
             (if (null? irritants)
                 (void)
                 (begin
                   (display " irritant: ")
                   (display (car irritants)))) ; TODO: what if there are more than one irritant?
             (newline)
             (k)))
     (void))))
  
