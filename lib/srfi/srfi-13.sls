(library (srfi :13)
  (export string-contains string-index)
  (import (for (jig clr reflection) run)
          (for (rnrs base) run)
          (for (rnrs base) expand))

  (define string-type (get-type "System.String"))

  (define index-of/char (procedure<-method (get-method string-type "IndexOf" (vector (get-type "System.Char")))))

  (define index-of/str (procedure<-method (get-method string-type "IndexOf" (vector string-type))))


  ;; TODO: implement optional arguments. Implement char-set and predicate as second argument)
  (define string-index
    (lambda (s x . rest)
      (if (null? rest)
          (cond ((char? x) (let ((index (index-of/char s x)))
                             (if (= index -1)
                                 #f
                                 index)))
                (else (error 'string-index "expected second argument to be a char." x)))
          (error 'string-index "optional arguments not implemented." rest))))

  (define string-contains/str?
    (procedure<-method (get-method string-type "Contains" (vector (get-type "System.String")))))

  (define string-contains/char?
    (procedure<-method (get-method string-type "Contains" (vector (get-type "System.Char")))))


  ;; TODO: string-contains has two pair of optional arguments -- two start and end ranges
  (define string-contains
    (lambda (s x . rest)
      (if (null? rest)
          (cond ((char? x)
                 (let ((index (index-of/char s x)))
                   (if (= index -1)
                       #f
                       index)))
                ((string? x)
                 (let ((index (index-of/str s x)))
                   (if (= index -1)
                       #f
                       index)))
                (else (error 'string-contains "expected second argument to be a string or char." x)))
          (error 'string-contains "optional arguments not implemented." rest)))))



