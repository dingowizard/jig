(library (rnrs base)
  (export ; 11.1 Base types
          boolean? symbol? char? vector? null? pair? number? string? procedure?
          ; 11.2 Definitions
          define define-syntax
          ; 11.4.1 Quotation
          quote
          ; 11.4.2 Procedures
          lambda
          ; 11.4.3 Conditionals
          if
          ; 11.4.4 Assignments
          set!
          ; 11.4.5 Derived Conditionals
          cond case and or
          ; 11.4.6 Binding constructs
          let let* letrec ; letrec* let-values let*-values
          ; 11.4.7 Sequencing
          begin
          ; 11.5 Equivalence Predicates
          eqv? ; eq? equal?
          ; 11.6 Procedure predicate NOTE: already exported under 11.1
          ; 11.7.4 Numerical operations
          ; number? NOTE: already exported 11.1
          ; complex? real? rational? integer? real-valued? rational-valued? integer-valued?
          ; exact? inexact? exact inexact
          ; Arithmetic operations
          = < > ; <= >=
          zero? positive? negative? odd? even? ; finite? infinite? nan?
          max ; min
          + * - /
          ; abs div-and-mod div mod div0 mod0 div0-and-mod0
          ; gcd lcm
          ; numerator denominator
          ; truncate floor ceiling round
          ;  rationalize
          ;  exp log sin cos tan asin acos atan
          ;  sqrt
          ;  exact-integer-sqrt
          ;  expt
          ;  real-part imag-part make-rectangular make-polar magnitude angle
          ;  number->string
          ;  string->number
          ;  11.8 Booleans
          ;  NOTE: boolean? already imported at 11.1
          not ; boolean=?
          ; 11.9 Pairs and lists
          ; NOTE: null? and pair? already imported
          cons car cdr caar cadr cdar cddr ; caaar caadr cadar cdaar
          caddr ; cdadr cddar
          cdddr ; caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr
          list? list length append reverse list-tail list-ref map for-each
          ; 11.10 Symbols
          ; NOTE: symbol? already exported
          ; symbol->string string->symbol symbol=?
          ; 11.11 Characters NOTE: char? already exported
          ;  integer->char char->integer char=? char<? char>? char<=? char>=?
          ; 11.12 Strings NOTE: string? already exported
          ;  string make-string string-length string-ref string=? string<? string>? string<=? string>=?
          ;  substring string-append list->string string->list string-copy string-for-each
          ; 11.13 Vectors NOTE: vector? already exported
          vector ; make-vector list->vector vector->list
          vector-length vector-ref ; vector-set! vector-fill!
          ; vector-map vector-for-each
          ; 11.14 Errors and violations
          error
          ; assert assertion-violation
          ; 11.15 Control features
          apply
          call-with-current-continuation call/cc
          values call-with-values
          dynamic-wind
          ; 11.16 Iteration NOTE: let already exported
          ; 11.17 Quasiquotation
          quasiquote
          ; 11.18 Binding constructs for syntactic keywords
          ; let-syntax letrec-syntax
          ; 11.19 Macro transformers
          syntax-rules) ; identifier-syntax)

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
