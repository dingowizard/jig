(library (jig)
  (export ; (rnrs base)
          length not
          map list error
          list-tail list-ref caar cadr cdar cddr caddr cdddr positive? negative? abs reverse for-each
          memv odd? even? let or let* letrec cond case
          cons car cdr append pair? number? char? string? procedure? list? null? zero? call/cc apply expand > < max min + * - / = eqv?
          boolean=?
          values call-with-values dynamic-wind symbol? vector vector-ref vector? vector-length
          and quasiquote syntax-rules
          define define-syntax begin lambda if set! quote quasiquote

          ; (rnrs records procedural)
          make-record-type-descriptor record-type-descriptor? make-record-constructor-descriptor
          record-predicate record-accessor record-constructor
          ; (rnrs records inspection)
          record?
          ; (rnrs lists)
          find exists for-all filter partition fold-left fold-right memv remv assv cons*
          ; (rnrs control)
          when unless do
          ; (rnrs exceptions)
          with-exception-handler raise raise-continuable ; guard
          ; (rnrs conditions) .. should really be (rnrs conditions (6)) whenever we get version numbers going
          condition? condition make-message-condition condition-message message-condition?
          ; (core-primitives)
          datum->syntax syntax->datum syntax->list syntax-e
          displayln display newline
          ; (jig prelude)
          all any void
          ; (jig parameters)
          make-parameter parameterize 
          ; (jig)
          template
          compose record-constructor-descriptor?)
  (import (for (core-primitives) run)
          (for (core-primitives) expand)
          (for (jig prelude) run)
          (for (jig prelude) expand)
          (for (jig parameters) run)
          (for (jig parameters) expand)
          (for (rnrs base) run)
          (for (rnrs base) expand)
          (for (rnrs control) run)
          (for (rnrs control) expand)
          (for (rnrs records procedural) run)
          (for (rnrs records procedural) expand)
          (for (rnrs records inspection) run)
          (for (rnrs records inspection) expand)
          (for (rnrs lists) run)
          (for (rnrs lists) expand)
          (for (rnrs conditions) run)
          (for (rnrs conditions) expand)
          (for (rnrs exceptions) run)
          (for (rnrs exceptions) expand))

  (define compose2
      (lambda (f1 f2)
        (lambda (x)
          (f2 (f1 x)))))

  (define compose
      (lambda xs
        (fold-left compose2 (lambda (x) x) xs))))
