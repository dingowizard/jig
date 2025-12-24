(library (jig)
  (export ; (jig prelude)
          length not caar cadr cdar cddr caddr cdddr fold-right map all any void error list
          ; (jig)
          list-tail list-ref positive? negative? abs fold-left filter partition compose reverse for-each
          memv odd? even? let or let* letrec cond case when do
          make-parameter parameterize
          ; (core-primitives)
          cons car cdr append pair? list? null? zero? call/cc + apply expand > < - * = eqv?
          values call-with-values dynamic-wind datum->syntax syntax->datum syntax->list syntax-e
          symbol? displayln display newline vector vector-ref vector? vector-length
          make-record-type-descriptor record-type-descriptor? make-record-constructor-descriptor record-constructor-descriptor?
          record? record-predicate record-accessor record-constructor
          and quasiquote syntax-rules)
  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand))

  (define list-tail
    (lambda (xs k)
      (if (= k 0)
          xs
          (list-tail (cdr xs) (- k 1)))))

  (define list-ref
    (lambda (xs k)
      (if (= k 0)
          (car xs)
          (list-ref (cdr xs) (- k 1)))))

  (define positive? (lambda (n) (> n 0)))

  (define negative? (lambda (n) (< n 0)))

  (define abs (lambda (n) (if (< n 0) (- n) n)))

  (define fold-left
    (lambda (fn acc xs)
      (if (null? xs)
          acc
          (fold-left fn (fn (car xs) acc) (cdr xs)))))


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

  (define compose2
      (lambda (f1 f2)
        (lambda (x)
          (f2 (f1 x)))))

  (define compose
      (lambda xs
        (fold-left compose2 (lambda (x) x) xs)))

  (define reverse
    (lambda (xs)
      (fold-left cons '() xs)))

  (define for-each
    (lambda (fn xs . rest)
      ((lambda (ls)
          (if (not (apply = (fold-right (lambda (x acc) (cons (length x) acc)) '() ls)))
              (error "for-each: lists must be same length" ls))
        (if (any null? ls)
            (void)
            (begin
              (apply fn (fold-right (lambda (x acc) (cons (car x) acc)) '() ls))
              (apply for-each (cons fn (fold-right (lambda (x acc) (cons (cdr x) acc)) '() ls)))))) (cons xs rest))))

  (define memv
     (lambda (x xs)
        (if (null? xs)
            #f
            (if (eqv? (car xs) x)
                xs
                (memv x (cdr xs))))))


  (define odd?
    (lambda (n)
      (if (= n 0)
          #f
          (even? (- n 1)))))

  (define even?
    (lambda (n)
      (if (= n 0)
          #t
          (odd? (- n 1)))))

  (define-syntax let
     (syntax-rules ()
        ((let ((p x) ...) body0 bodies ...) ((lambda (p ...) body0 bodies ...) x ...))))

  (define-syntax or
     (syntax-rules ()
        ((or) #f)
        ((or x) x)
        ((or a rest ...) (let ((tmp a)) (if tmp tmp (or rest ...))))))


  (define-syntax let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
       (let () body1 body2 ...))
      ((let* ((name1 expr1) (name2 expr2) ...) body1 body2 ...)
       (let ((name1 expr1))
         (let* ((name2 expr2) ...) body1 body2 ...)))))


  (define-syntax letrec
     (syntax-rules ()
        ((letrec ((p x) ...) body0 bodies ...) ((lambda () (define p x) ... body0 bodies ...)))))

  (define-syntax cond
    (syntax-rules (else =>)
      ((cond (else result1 result2 ...))
       (begin result1 result2 ...))
      ((cond (test => result))
       (let ((temp test))
         (if temp (result temp))))
      ((cond (test => result) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             (result temp)
             (cond clause1 clause2 ...))))
      ((cond (test)) test)
      ((cond (test) clause1 clause2 ...)
       (let ((temp test))
         (if temp
             temp
             (cond clause1 clause2 ...))))
      ((cond (test result1 result2 ...))
       (if test (begin result1 result2 ...)))
      ((cond (test result1 result2 ...)
             clause1 clause2 ...)
       (if test
           (begin result1 result2 ...)
           (cond clause1 clause2 ...)))))

  ;; ; this is case from r5rs. The one from r6rs has stuff after ... in a pattern, which we can't handle yet
  (define-syntax case
    (syntax-rules (else)
      ((case (key ...)
         clauses ...)
       (let ((atom-key (key ...)))
         (case atom-key clauses ...)))
      ((case key
         (else result1 result2 ...))
       (begin result1 result2 ...))
      ((case key
         ((atoms ...) result1 result2 ...))
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)))
      ((case key
         ((atoms ...) result1 result2 ...)
         clause clauses ...)
       (if (memv key '(atoms ...))
           (begin result1 result2 ...)
           (case key clause clauses ...)))))

  (define-syntax when
     (syntax-rules ()
        ((when test body0 bodies ...) (if test (begin body0 bodies ...)))))

  ;; ; TODO: make step optional (see spec r6rs-lib)
  (define-syntax do
     (syntax-rules ()
        ((do ((var init step) ...)
             (test expr ...)
             command ...)
         ((lambda ()
             (define loop (lambda (var ...)
                             (if test
                                 (begin
                                    (void)
                                    expr ...)
                                 (begin
                                    command ...
                                    (loop step ...)))))
             (loop init ...))))))

  (define make-parameter
    (lambda (init . o)
      (let* ((converter (if (not (null? o)) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (if (null? args)
              value
              (let ((len (length args)))
                (cond ((= len 1)
                       (set! value (converter (car args))))
                      ((= len 2)
                       (set! value ((cadr args) (car args))))
                      (#t 'error))))))))

  (define-syntax parameterize
     (syntax-rules ()
        ((parameterize ((p0 v0) (p v) ...) body0 body ...)
         ((lambda olds
             (dynamic-wind
                (lambda () (p0 v0) (p v) ...)
                (lambda () body0 body ...)
                (lambda () (for-each (lambda (pr old) (pr old (lambda (x) x))) (list p0 p ...) olds)))) (p0) (p) ...))))
  )
