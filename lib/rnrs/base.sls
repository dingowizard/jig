(library (rnrs base)
  (export boolean? not symbol? ; symbol->string string->symbol
          ; char?  char=? char<? char>? char<=? char>=? integer->char char->integer
          list? null? pair? cons car cdr caar cadr cdar cddr ; caaar caadr cadar cdaar
          caddr ; cdadr cddar
          cdddr ; caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr
          number?
         ;  string?
          procedure?
          ; TODO: it should be possible to export these core syntactic forms, but it's not because they are automatically included in every environment
          ; define set! define-syntax ; let-syntax letrec-syntax identifier-syntax
          syntax-rules ; lambda
          let let* letrec ; letrec* let-values let*-values
          ; begin
          ; quote
          quasiquote
          ; unquote unquote-splicing ; TODO: why does base eport these if they have no meaning at top-level?
          ; if
          cond case and or ; eq?
          eqv? ; equal? symbol=? complex? real-part imag-part make-rectangular make-polar magnitude angle sqrt exp expt log sin cos tan asin acos atan real? rational? numerator denominator rationalize
               ; exact? inexact?
               ; exact inexact
               ; integer?
          odd? even? ; gcd lcm exact-integer-sqrt
          = < > ; <= >=
          zero? positive? negative?
          for-each list error length list-ref list-tail append reverse
          ; number->string string->number
          ; string make-string
          ; list->string string->list string-length string-ref string-copy substring string=? string<? string>? string<=? string>=? string-append string-for-each
          + - * ; / max min abs truncate floor ceiling round div mod div-and-mod div0 mod0 div0-and-mod0 real-valued? rational-valued? nan? infinite? finite?
                ; assert
                ; error assertion-violation vector-map vector-for-each
          vector vector? ; make-vector list->vector vector->list
          vector-length vector-ref ; vector-set! vector-fill!
          call-with-current-continuation call/cc values call-with-values dynamic-wind apply)

  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand))
  (define boolean? ; TODO: is eqv? the right test?
    (lambda (x)
      (if (eqv? x #t)
          #t
          (if (eqv? x #f)
              #t
              #f))))

  (define call-with-current-continuation call/cc)

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


  (define caar (lambda (p) (car (car p))))

  (define cadr (lambda (p) (car (cdr p))))

  (define cdar (lambda (p) (cdr (car p))))

  (define cddr (lambda (p) (cdr (cdr p))))

  (define caddr (lambda (p) (car (cdr (cdr p)))))

  (define cdddr (lambda (p) (cdr (cdr (cdr p)))))

  (define positive? (lambda (n) (> n 0)))

  (define negative? (lambda (n) (< n 0)))

  ; TODO: cute. but make efficient versions of odd and even
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

  (define abs (lambda (n) (if (< n 0) (- n) n)))

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
           (case key clause clauses ...))))))
