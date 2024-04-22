(define list (lambda xs xs))

(define length
    (lambda (l)
      (define loop
        (lambda (acc l)
          (if (null? l)
              acc
              (loop (+ 1 acc) (cdr l)))))
      (loop 0 l)))

(define list-tail
  (lambda (x k)
    (if (= k 0)
        x
        (list-tail (cdr x) (- k 1)))))

(define list-ref
  (lambda (xs k)
    (if (= k 0)
        (car xs)
        (list-ref (cdr xs) (- k 1)))))

(define zero? (lambda (x) (= x 0)))

(define not (lambda (x) (if x #f #t)))

(define list?
  (lambda (x)
    (if (null? x)
        #t
        (if (pair? x)
            (list? (cdr x))
            #f))))

(define fold
  (lambda (fn init xs)
    (if (null? xs)
        init
        (fold fn (fn (car xs) init) (cdr xs)))))

(define reverse
  (lambda (xs)
    (fold cons (list) xs)))

;; TODO: why do we need to define map1? we get infinite loop otherwise
;; TODO: more efficient any-null? (short circuit with call/cc?)
(define map
  (lambda (fn xs . rest)
    (define any-null?
      (lambda (xs)
        (fold (lambda (l acc) (if acc acc (if (null? l) #t #f)))
              #f
              xs)))
    (define map1
      (lambda (fn xs)
        (if (null? xs)
            '()
            (cons (fn (car xs)) (map fn (cdr xs))))))
    ((lambda (ls)
      (if (any-null? ls)
          '()
          (cons (apply fn (map1 car ls))
                (apply map (cons fn (map1 cdr ls)))))) (cons xs rest))))

;; TODO: support any number of list arguments, including no args
(define append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2)))))

(define caar (lambda (p) (car (car p))))

(define cadr (lambda (p) (car (cdr p))))

(define cdar (lambda (p) (cdr (car p))))

(define cddr (lambda (p) (cdr (cdr p))))

(define caddr (lambda (p) (car (cdr (cdr p)))))

(define cdddr (lambda (p) (cdr (cdr (cdr p)))))

(define-syntax begin
  (lambda (stx)
    (datum->syntax
     stx
     (list
      (append (list 'lambda (list))
              (cdr (syntax->list stx)))))))

(define-syntax let
  (lambda (stx)
    (datum->syntax
     stx
     (cons
       (append (list 'lambda
                     (map (lambda (b) (car (syntax->list b))) (syntax->list (cadr (syntax->list stx)))))
               (cddr (syntax->list stx)))
       (map (lambda (b) (cadr (syntax->list b))) (syntax->list (cadr (syntax->list stx))))))))

(define-syntax or
  (lambda (stx)
    (let ((stx-list (syntax->list stx)))
      (if (= 1 (length stx-list))
          (datum->syntax stx (quote #f))
          (datum->syntax
           stx
           (list 'let
                 (list (list 'x (car (cdr stx-list))))
                 (list 'if 'x 'x (append (list 'or) (cdr (cdr stx-list))))))))))

(define-syntax let*
  (lambda (stx)
    (let ((syntax-list (syntax->list stx)))
      (let ((bindings (syntax->list (cadr syntax-list))))
        (datum->syntax
         stx
         (let ((len (length bindings))
               (body (cddr syntax-list)))
          (if (= len 0)
              `((lambda () ,@body))
              (if (= len 1)
                  `(let (,(car bindings)) ,@body)
                  `(let (,(car bindings)) (let* ,(cdr bindings) ,@body))))))))))

(define void (lambda () (if #f #f)))

(define-syntax letrec
  (lambda (stx)
    (let* ((stx-list (syntax->list stx))
           (bs (map syntax->list (syntax->list (cadr stx-list))))
           (ps (map car bs))
           (vs (map cadr bs))
           (body (cddr stx-list)))
      (datum->syntax
       stx
       `((lambda () ,@(map (lambda (p v) `(define ,p ,v)) ps vs) ,@body))))))

;; TODO: figure out why these two definitions of letrec, which work in racket, fail in Jig
;; (letrec ((x 1)) x) => error: unbound x
;; the x in the body can't find its binding because its only scope is the toplevel scope
;;
;; (define-syntax letrec
;;   (lambda (stx)
;;     (let* ((stx-list (syntax->list stx))
;;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;;            (ps (map car bs))
;;            (vs (map cadr bs))
;;            (body (cddr stx-list)))
;;       (datum->syntax
;;        stx
;;        `(let ,(map (lambda (p) `(,p (void))) ps)
;;           ,@(map (lambda (p v) `(set! ,p ,v)) ps vs)
;;           ,@body)))))

;; (define-syntax letrec
;;   (lambda (stx)
;;     (let* ((stx-list (syntax->list stx))
;;            (bs (map syntax->list (syntax->list (cadr stx-list))))
;;            (ps (map car bs))
;;            (vs (map cadr bs))
;;            (body (cddr stx-list)))
;;       (datum->syntax
;;        stx
;;        `((lambda (,@ps)
;;           ,@(map (lambda (p v) `(set! ,p ,v)) ps vs)
;;           ,@body) ,@(map (lambda (p) `(if #f #f)) ps))))))

(define-syntax cond
  (lambda (stx)
    (let ((clauses (cdr (syntax->list stx))))
      (let ((clause1 (syntax->list (car clauses))))
        (datum->syntax
         stx
         (if (= (length clauses) 1)
             `(if ,(car clause1)
                  ,(cadr clause1))
             `(if ,(car clause1)
                  ,(cadr clause1)
                  (cond ,@(cdr clauses)))))))))

(define-syntax when
  (lambda (stx)
    (let* ((stx-list (syntax->list stx))
           (condition (cadr stx-list))
           (body (cddr stx-list)))
      (datum->syntax stx `(if ,condition (begin ,@body))))))

(define-syntax do
  (lambda (stx)
    (let* ((stx-list (syntax->list stx))
           (bs (map syntax->list (syntax->list (cadr stx-list))))
           (ps (map car bs))
           (inits (map cadr bs))
           (incrs (map caddr bs))
           (test (car (syntax->list (caddr stx-list))))
           (result (if (null? (cdr (syntax->list (caddr stx-list))))
                       (syntax (void))
                       (cadr (syntax->list (caddr stx-list)))))
           (body (cdddr stx-list)))
      (datum->syntax
       stx
       `((lambda () (define loop (lambda ,ps (if ,test ,result (begin ,@body (loop ,@incrs))))) (loop ,@inits)))))))


(define dynamic-wind #f)

(let ((winders '()))
  (define common-tail
    (lambda (x y)
      (let ((lx (length x)) (ly (length y)))
        (do ((x (if (> lx ly) (list-tail x (- lx ly)) x) (cdr x))
             (y (if (> ly lx) (list-tail y (- ly lx)) y) (cdr y)))
            ((eq? x y) x)))))
  (define do-wind
    (lambda (new)
      (let ((tail (common-tail new winders)))
        (define f
          (lambda (l)
            (if (not (eq? l tail))
                (begin
                  (set! winders (cdr l))
                  ((cdar l))
                  (f (cdr l))))))
        (f winders)
        (define f
          (lambda (l)
            (if (not (eq? l tail))
                (begin
                  (f (cdr l))
                  ((caar l))
                  (set! winders l)))))
        (f new))))
  (set! call/cc
    (let ((prim-callcc call/cc))
      (lambda (f)
        (prim-callcc (lambda (k)
                      (f (let ((save winders))
                           (lambda xs
                             (if (not (eq? save winders)) (do-wind save))
                             (apply k xs)))))))))
  (set! call-with-current-continuation call/cc)
  ;; (set! error
  ;;   (let ((prim-error error))
  ;;     (lambda (msg . xs)
  ;;       (define run-all
  ;;         (lambda (ws)
  ;;           (if (null? ws)
  ;;               '()
  ;;               (begin
  ;;                 ((cdar ws))
  ;;                 (run-all (cdr ws))))))
  ;;       ; do all out thunks before exiting
  ;;       (run-all winders)
  ;;       (apply prim-error (cons msg xs)))))
  (set! dynamic-wind
    (lambda (in body out)
      (in)
      (set! winders (cons (cons in out) winders))
      (call-with-values
          (lambda () (body))
        (lambda ans
          (set! winders (cdr winders))
          (out)
          (apply values ans))))))

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

; TODO: parameterize should handle multiple bindings
; TODO: in below, it shold be possible to write `(lambda () . ,bodies), but it expands incorrectly
(define-syntax parameterize
  (lambda (stx)
    (let* ((stx-list (syntax->list stx))
           (bs (map syntax->list (syntax->list (cadr stx-list))))
           (ps (map car bs))
           (vs (map cadr bs))
           (bodies (cddr stx-list)))
      (datum->syntax
       stx
       `((lambda (old)
           (dynamic-wind
             (lambda () (,(car ps) ,(car vs)))
             (lambda () ,@bodies)
             (lambda () (,(car ps) old (lambda (x) x))))) (,(car ps)))))))


(define with-exception-handler #f)
(define raise #f)
(define raise-continuable #f)

(call/cc
 (lambda (k)
   (define *current-exception-handlers*
     (list (lambda (condition)
             (display "unandled exception ")
             (display condition)
             (newline)
             (k (void)))))
    (define with-exception-handlers
        (lambda (new-handlers thunk)
            (let ((previous-handlers *current-exception-handlers*))
            (dynamic-wind
                (lambda ()
                (set! *current-exception-handlers* new-handlers))
                thunk
                (lambda ()
                (set! *current-exception-handlers* previous-handlers))))))
    (set! with-exception-handler
        (lambda (handler thunk)
            (with-exception-handlers (cons handler *current-exception-handlers*)
                                    thunk)))
    (set! raise
        (lambda (obj)
            (let ((handlers *current-exception-handlers*))
            (with-exception-handlers (cdr handlers)
                (lambda ()
                ((car handlers) obj)
                (abort "user-defined handler returned on non-continuable exception"
                        (car handlers)
                        obj))))))
    (set! raise-continuable
        (lambda (obj)
            (let ((handlers *current-exception-handlers*))
            (with-exception-handlers (cdr handlers)
                (lambda ()
                ((car handlers) obj))))))))

(define error #f)

(call/cc
 (lambda (k)
   (set! error
         (lambda (msg . xs)
           (display msg)
           (newline)
           (k (void))))))
