(library (jig)
  (export ; (rnrs base)
          boolean? symbol? char? vector? null? pair? number? string? procedure?
          define define-syntax quote lambda if set! cond case and or
          let let* letrec ; letrec* let-values let*-values
          begin
          eqv? ; eq? equal?
          ; complex? real? rational?
          integer? ; real-valued? rational-valued? integer-valued? exact? inexact? exact inexact
          = < > ; <= >=
          zero? positive? negative? odd? even? ; finite? infinite? nan?
          max min + * - /
          abs
          div-and-mod div mod  div0 ; mod0
          div0-and-mod0 ; gcd lcm numerator denominator
          truncate floor ceiling round ; rationalize exp log sin cos tan asin acos atan
          sqrt ; exact-integer-sqrt
          expt ; real-part imag-part make-rectangular make-polar magnitude angle
          ;  number->string string->number
          not boolean=?
          cons car cdr caar cadr cdar cddr ; caaar caadr cadar cdaar
          caddr ; cdadr cddar
          cdddr ; caaaar caaadr caadar cadaar cdaaar cddaar cdadar cdaadr cadadr caaddr caddar cadddr cdaddr cddadr cdddar cddddr
          list? list length append reverse list-tail list-ref map for-each
          ; symbol->string string->symbol symbol=?
          ;  integer->char char->integer char=? char<? char>? char<=? char>=?
          ;  string make-string string-length string-ref string=? string<? string>? string<=? string>=?
          ;  substring string-append list->string string->list string-copy string-for-each
          vector ; make-vector list->vector vector->list
          vector-length vector-ref ; vector-set! vector-fill!
          ; vector-map vector-for-each
          error
          ; assert assertion-violation
          apply call-with-current-continuation call/cc
          values call-with-values dynamic-wind quasiquote ; let-syntax letrec-syntax
          syntax-rules

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
          ; (rnrs programs)
          exit
          ; (core-primitives)
          datum->syntax syntax->datum syntax->list syntax-e
          displayln display newline
          ; (jig prelude)
          all any void
          ; (jig parameters)
          make-parameter parameterize 
          ; (jig)
          template
          compose record-constructor-descriptor?
          ;; TODO: doesn't some rnrs library export import?
          import)
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
          (for (rnrs exceptions) expand)
          (for (rnrs programs) run))

  (define compose2
      (lambda (f1 f2)
        (lambda (x)
          (f2 (f1 x)))))

  (define compose
      (lambda xs
        (fold-left compose2 (lambda (x) x) xs))))
