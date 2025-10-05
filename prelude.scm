(define list (lambda xs xs))

; (define length
;    (lambda (xs)
;       (if (null? xs)
;           0
;           (+ 1 (length (cdr xs))))))

;; (define list?
;;   (lambda (x)
;;     (if (null? x)
;;         #t
;;         (if (pair? x)
;;             (list? (cdr x))
;;             #f))))

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

;; (define zero? (lambda (x) (= x 0)))

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

;; (define member
;;    (lambda (x xs)
;;       (if (null? xs)
;;           #f
;;           (if (equal? (car xs) x)
;;               xs
;;               (member x (cdr xs))))))
;;
;; (define-syntax macro
;;   (lambda (stx)
;;     (datum->syntax
;;      stx
;;      (car (cdr (syntax->list stx))))))

;; (define-syntax or2
;;     (lambda (stx)
;;       (datum->syntax stx
;;         `((lambda (x) (if x x ,(car (cdr (cdr (syntax->list stx)))))) ,(car (cdr (syntax->list stx)))))))
;; (define memq
;;    (lambda (x xs)
;;       (if (null? xs)
;;           #f
;;           (if (eq? (car xs) x)
;;               xs
;;               (memq x (cdr xs))))))

; doesn't use quasiquote
; (define-syntax let
;   (lambda (stx)
;     (datum->syntax
;      stx
;      (cons
;        (append (list 'lambda
;                      (map (lambda (b) (car (syntax->list b))) (syntax->list (cadr (syntax->list stx)))))
;                (cddr (syntax->list stx)))
;        (map (lambda (b) (cadr (syntax->list b))) (syntax->list (cadr (syntax->list stx))))))))

;; (define-syntax let
;;    (lambda (stx)
;;     ((lambda (xs)
;;         ((lambda (bindings bodies)
;;             (datum->syntax
;;                stx
;;                `((lambda ,(map car bindings) ,@bodies) ,@(map cadr bindings))))
;;             ;; (map syntax->list (syntax->list (cadr xs)))
;;             ;; (cddr xs)))
;;             (map syntax->list (syntax->list (car (cdr xs))))
;;             (cdr (cdr xs))))
;;         (syntax->list stx))))

;; (define odd?
;;    (lambda (n)
;;       (not (= (mod n 2) 0))))

;; TODO: even?, odd? are here as trivial examples of mutual recursion for testing purposes
;; replace with efficient versions later
(define odd?
  (lambda (n)
    (if (= n 0)
        #f
        (even? (- n 1)))))

;; (define even?
;;    (lambda (n)
;;       (= (mod n 2) 0)))

(define even?
  (lambda (n)
    (if (= n 0)
        #t
        (odd? (- n 1)))))

; note: syntax-rules depends on all
;; (define all
;;    (lambda (pred xs)
;;       (if (null? xs)
;;           #t
;;           (if (pred (car xs)) (all pred (cdr xs)) #f))))


;; (define-syntax let
;;    (syntax-rules ()
;;       ((let ((p x) ...) body0 bodies ...) ((lambda (p ...) body0 bodies ...) x ...))))

; --- or that doesn't use match
; (define-syntax or
;   (lambda (stx)
;     (let ((stx-list (syntax->list stx)))
;       (if (= 1 (length stx-list))
;           (datum->syntax stx (quote #f))
;           (datum->syntax
;            stx
;            (list 'let
;                  (list (list 'x (car (cdr stx-list))))
;                  (list 'if 'x 'x (append (list 'or) (cdr (cdr stx-list))))))))))

; (define-syntax or
;    (lambda (stx)
;       (datum->syntax
;          stx
;          (match (cdr (syntax->list stx))
;             ('() #f)
;             ((x) x)
;             ((x . rest) `(let ((tmp ,x)) (if tmp tmp (or ,@rest))))))))
;; (define-syntax or
;;    (syntax-rules ()
;;       ((or) #f)
;;       ((or x) x)
;;       ((or a rest ...) (let ((tmp a)) (if tmp tmp (or rest ...))))))

;; (define-syntax let*
;;   (lambda (stx)
;;     (let ((syntax-list (syntax->list stx)))
;;       (let ((bindings (syntax->list (cadr syntax-list))))
;;         (datum->syntax
;;          stx
;;          (let ((len (length bindings))
;;                (body (cddr syntax-list)))
;;           (if (= len 0)
;;               `((lambda () ,@body))
;;               (if (= len 1)
;;                   `(let (,(car bindings)) ,@body)
;;                   `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))))))


;; (define-syntax let*
;;   (syntax-rules ()
;;     ((let* () body1 body2 ...)
;;      (let () body1 body2 ...))
;;     ((let* ((name1 expr1) (name2 expr2) ...) body1 body2 ...)
;;      (let ((name1 expr1))
;;        (let* ((name2 expr2) ...) body1 body2 ...)))))

; (define-syntax letrec
;   (lambda (stx)
;     (let* ((stx-list (syntax->list stx))
;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;            (ps (map car bs))
;            (vs (map cadr bs))
;            (body (cddr stx-list)))
;       (datum->syntax
;        stx
;        `((lambda () ,@(map (lambda (p v) `(define ,p ,v)) ps vs) ,@body))))))

;; (define-syntax letrec
;;    (syntax-rules ()
;;       ((letrec ((p x) ...) body0 bodies ...) ((lambda () (define p x) ... body0 bodies ...)))))
;----------------------------------------------------------------------------------------
; LEAVE COMMENTED
; ;; TODO: figure out why these two definitions of letrec, which work in racket, fail in Jig
; ;; Is it because of nested back-tick??
; ;; (letrec ((x 1)) x) => error: unbound x
; ;; the x in the body can't find its binding because its only scope is the toplevel scope
; ;;
; (define-syntax letrec
;   (lambda (stx)
;     (let* ((stx-list (syntax->list stx))
;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;            (ps (map car bs))
;            (vs (map cadr bs))
;            (body (cddr stx-list)))
;       (datum->syntax
;        stx
;        `(let ,(map (lambda (p) `(,p (void))) ps)
;           ,@(map (lambda (p v) `(set! ,p ,v)) ps vs)
;           ,@body)))))

; ;; (define-syntax letrec
; ;;   (lambda (stx)
; ;;     (let* ((stx-list (syntax->list stx))
; ;;            (bs (map syntax->list (syntax->list (cadr stx-list))))
; ;;            (ps (map car bs))
; ;;            (vs (map cadr bs))
; ;;            (body (cddr stx-list)))
; ;;       (datum->syntax
; ;;        stx
; ;;        `((lambda (,@ps)
; ;;           ,@(map (lambda (p v) `(set! ,p ,v)) ps vs)
; ;;           ,@body) ,@(map (lambda (p) `(if #f #f)) ps))))))
; ------------------------------------------------------------------------------

; TODO: support 'else'
; (define-syntax cond
;   (lambda (stx)
;     (let ((clauses (cdr (syntax->list stx))))
;       (let ((clause1 (syntax->list (car clauses))))
;         (datum->syntax
;          stx
;          (if (= (length clauses) 1)
;              `(if ,(car clause1)
;                   ,(cadr clause1))
;              `(if ,(car clause1)
;                   ,(cadr clause1)
;                   (cond ,@(cdr clauses)))))))))

; (define-syntax cond
;    (lambda (stx)
;       (datum->syntax
;          stx
;          (match (cdr (syntax->datum stx))
;             (((test x)) `(if ,test ,x))
;             (((test x) . more) `(if ,test ,x (cond ,@more)))))))

; (define-syntax cond
;   (lambda (stx)
;     (datum->syntax
;       stx
;       (match-syntax stx
;         ((cond (test expr))
;         `(if ,test ,expr))
;         ((cond (test expr) . more)
;         `(if ,test ,expr (cond ,@more)))))))
;
; TODO: let-values from spec


;; (define-syntax cond
;;   (syntax-rules (else =>)
;;     ((cond (else result1 result2 ...))
;;      (begin result1 result2 ...))
;;     ((cond (test => result))
;;      (let ((temp test))
;;        (if temp (result temp))))
;;     ((cond (test => result) clause1 clause2 ...)
;;      (let ((temp test))
;;        (if temp
;;            (result temp)
;;            (cond clause1 clause2 ...))))
;;     ((cond (test)) test)
;;     ((cond (test) clause1 clause2 ...)
;;      (let ((temp test))
;;        (if temp
;;            temp
;;            (cond clause1 clause2 ...))))
;;     ((cond (test result1 result2 ...))
;;      (if test (begin result1 result2 ...)))
;;     ((cond (test result1 result2 ...)
;;            clause1 clause2 ...)
;;      (if test
;;          (begin result1 result2 ...)
;;          (cond clause1 clause2 ...)))))

; this is case from r5rs. The one from r6rs has stuff after ... in a pattern, which we can't handle yet
;; (define-syntax case
;;   (syntax-rules (else)
;;     ((case (key ...)
;;        clauses ...)
;;      (let ((atom-key (key ...)))
;;        (case atom-key clauses ...)))
;;     ((case key
;;        (else result1 result2 ...))
;;      (begin result1 result2 ...))
;;     ((case key
;;        ((atoms ...) result1 result2 ...))
;;      (if (memv key '(atoms ...))
;;          (begin result1 result2 ...)))
;;     ((case key
;;        ((atoms ...) result1 result2 ...)
;;        clause clauses ...)
;;      (if (memv key '(atoms ...))
;;          (begin result1 result2 ...)
;;          (case key clause clauses ...)))))
;
; (define-syntax when
;   (lambda (stx)
;     (let* ((stx-list (syntax->list stx))
;            (condition (cadr stx-list))
;            (body (cddr stx-list)))
;       (datum->syntax stx `(if ,condition (begin ,@body))))))

; (define-syntax when
;    (lambda (stx)
;       (datum->syntax
;          stx
;          (match (cdr (syntax->list stx))
;             ((test . bodies) `(if ,test (begin ,@bodies)))))))

;; (define-syntax when
;;    (syntax-rules ()
;;       ((when test body0 bodies ...) (if test (begin body0 bodies ...)))))

; (define-syntax do
;   (lambda (stx)
;     (let* ((stx-list (syntax->list stx))
;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;            (ps (map car bs))
;            (inits (map cadr bs))
;            (incrs (map caddr bs))
;            (test (car (syntax->list (caddr stx-list))))
;            (result (if (null? (cdr (syntax->list (caddr stx-list))))
;                        (quote-syntax (void))
;                        (cadr (syntax->list (caddr stx-list)))))
;            (body (cdddr stx-list)))
;       (datum->syntax
;        stx
;        `((lambda () (define loop (lambda ,ps (if ,test ,result (begin ,@body (loop ,@incrs))))) (loop ,@inits)))))))

; (define-syntax do
;    (lambda (stx)
;       (datum->syntax
;          stx
;          (match-syntax stx
;             ((do ((i init incr) . vars)
;                  (test result))
;              `((lambda ()
;                   (define loop
;                      (lambda ,(cons i (map (compose car syntax-e) vars))
;                         (if ,test
;                             ,result
;                             (loop ,@(cons incr (map (compose caddr syntax-e) vars))))))
;                   (loop ,@(cons init (map (compose cadr syntax-e) vars))))))
;             ((do ((i init incr) . vars)
;                  (test result) . bodies)
;              `((lambda ()
;                   (define loop
;                      (lambda ,(cons i (map (lambda (x) (car (syntax-e x))) vars))
;                         (if ,test
;                             ,result
;                             (begin ,@bodies
;                                    (loop ,@(cons incr (map (lambda (x) (caddr (syntax-e x))) vars)))))))
;                   (loop ,@(cons init (map (lambda (x) (cadr (syntax-e x))) vars))))))))))
; TODO: make step optional (see spec r6rs-lib)
;; (define-syntax do
;;    (syntax-rules ()
;;       ((do ((var init step) ...)
;;            (test expr ...)
;;            command ...)
;;        ((lambda ()
;;            (define loop (lambda (var ...)
;;                            (if test
;;                                (begin
;;                                   (void)
;;                                   expr ...)
;;                                (begin
;;                                   command ...
;;                                   (loop step ...)))))
;;            (loop init ...))))))


;; (define dynamic-wind #f)

;; (let ((winders '()))
;;   (define common-tail
;;     (lambda (x y)
;;       (let ((lx (length x)) (ly (length y)))
;;         (do ((x (if (> lx ly) (list-tail x (- lx ly)) x) (cdr x))
;;              (y (if (> ly lx) (list-tail y (- ly lx)) y) (cdr y)))
;;             ((eq? x y) x)))))
;;   (define do-wind
;;     (lambda (new)
;;       (let ((tail (common-tail new winders)))
;;         (define f
;;           (lambda (l)
;;             (if (not (eq? l tail))
;;                 (begin
;;                   (set! winders (cdr l))
;;                   ((cdar l))
;;                   (f (cdr l))))))
;;         (f winders)
;;         (define f
;;           (lambda (l)
;;             (if (not (eq? l tail))
;;                 (begin
;;                   (f (cdr l))
;;                   ((caar l))
;;                   (set! winders l)))))
;;         (f new))))
;;   (set! call/cc
;;     (let ((prim-callcc call/cc))
;;       (lambda (f)
;;         (prim-callcc (lambda (k)
;;                       (f (let ((save winders))
;;                            (lambda xs
;;                              (if (not (eq? save winders)) (do-wind save))
;;                              (apply k xs)))))))))
;;   (set! call-with-current-continuation call/cc)
;;   (set! dynamic-wind
;;     (lambda (in body out)
;;       (in)
;;       (set! winders (cons (cons in out) winders))
;;       (call-with-values
;;           (lambda () (body))
;;         (lambda ans
;;           (set! winders (cdr winders))
;;           (out)
;;           (apply values ans))))))

;; (define make-parameter
;;   (lambda (init . o)
;;     (let* ((converter (if (not (null? o)) (car o) (lambda (x) x)))
;;            (value (converter init)))
;;       (lambda args
;;         (if (null? args)
;;             value
;;             (let ((len (length args)))
;;               (cond ((= len 1)
;;                      (set! value (converter (car args))))
;;                     ((= len 2)
;;                      (set! value ((cadr args) (car args))))
;;                     (#t 'error))))))))

; (define-syntax parameterize
;   (lambda (stx)
;     (let* ((stx-list (syntax->list stx))
;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;            (ps (map car bs))
;            (vs (map cadr bs))
;            (bodies (cddr stx-list)))
;       (datum->syntax
;        stx
;        `((lambda (old)
;            (dynamic-wind
;              (lambda () (,(car ps) ,(car vs)))
;              (lambda () ,@bodies)
;              (lambda () (,(car ps) old (lambda (x) x))))) (,(car ps)))))))

; TODO: make tests for make-parameter and parameterize
;; (define-syntax parameterize
;;    (syntax-rules ()
;;       ((parameterize ((p0 v0) (p v) ...) body0 body ...)
;;        ((lambda olds
;;            (dynamic-wind
;;               (lambda () (p0 v0) (p v) ...)
;;               (lambda () body0 body ...)
;;               (lambda () (map (lambda (pr old) (pr old (lambda (x) x))) (list p0 p ...) olds)))) (p0) (p) ...))))

;; (define with-exception-handler #f)
;; (define raise #f)
;; (define raise-continuable #f)

;; (call/cc
;;  (lambda (k)
;;    (define *current-exception-handlers*
;;      (list (lambda (condition)
;;              (display "unhandled exception ")
;;              (display condition)
;;              (newline)
;;              (k (void)))))
;;    (define with-exception-handlers
;;        (lambda (new-handlers thunk)
;;            (let ((previous-handlers *current-exception-handlers*))
;;             (dynamic-wind
;;                 (lambda ()
;;                  (set! *current-exception-handlers* new-handlers))
;;                 thunk
;;                 (lambda ()
;;                  (set! *current-exception-handlers* previous-handlers))))))
;;    (set! with-exception-handler
;;        (lambda (handler thunk)
;;            (with-exception-handlers (cons handler *current-exception-handlers*)
;;                                    thunk)))
;;    (set! raise
;;        (lambda (obj)
;;            (let ((handlers *current-exception-handlers*))
;;             (with-exception-handlers (cdr handlers)
;;                 (lambda ()
;;                  ((car handlers) obj)
;;                  (abort "user-defined handler returned on non-continuable exception"
;;                          (car handlers)
;;                          obj))))))
;;    (set! raise-continuable
;;        (lambda (obj)
;;            (let ((handlers *current-exception-handlers*))
;;             (with-exception-handlers (cdr handlers)
;;                 (lambda ()
;;                  ((car handlers) obj))))))))

