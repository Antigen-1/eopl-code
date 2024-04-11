#lang racket/base
(require racket/contract racket/generic)
(provide (contract-out (rename remq remove-first (-> symbol? (listof symbol?) (listof symbol?)))
                       (occurs-free? (-> variable? LcExp? boolean?))
                       (subst (-> symbol? symbol? (listof s-exp?) (listof s-exp?))))
         s-exp?
         gen:LcExp-datum LcExp-datum/c LcExp-datum?
         variable? lambda? app? LcExp? variable-symbol lambda-symbol lambda-body app-operand app-operator
         (rename-out (length list-length)
                     (list-ref nth-element)))

#; (-> any/c exact-nonnegative-integer? boolean?)
;; Accepts a list l and a nonnegative integer n and find out whether l is a n-element list
(define (list-of-length? l n)
  (and (list? l) (list-of-length/unsafe? l n)))
(define (list-of-length/unsafe? l n)
  (or (and (zero? n) (null? l))
      (and (not (null? l)) (list-of-length/unsafe? (cdr l) (sub1 n)))))

;; LcExp Interfaces
#; (-> any/c boolean?)
;; Predicates for the default representation
(define (maybe-default-LcExp? v) (or (symbol? v) (list? v)))
(define (default-variable? v) (symbol? v))
(define (default-lambda? v)
  (and (list-of-length? v 3)
       (eq? (car v) 'lambda)
       (list-of-length? (cadr v) 1)
       (symbol? (caadr v))
       (default-LcExp? (caddr v))))
(define (default-app? v)
  (and (list-of-length? v 2)
       (andmap default-LcExp? v)))
(define (default-LcExp? v)
  (or (default-variable? v)
      (default-lambda? v)
      (default-app? v)))
;; Fallback predicates
(define (fallback-LcExp? v)
  (or (variable? v) (lambda? v) (app? v)))
;; Defines gen:LcExp-datum
(define-generics LcExp-datum
  [variable? LcExp-datum]
  [lambda? LcExp-datum]
  [app? LcExp-datum]
  [LcExp? LcExp-datum]
  [variable-symbol LcExp-datum]
  [lambda-symbol LcExp-datum]
  [lambda-body LcExp-datum]
  [app-operand LcExp-datum]
  [app-operator LcExp-datum]
  #:fallbacks [(define LcExp? fallback-LcExp?)]
  #:fast-defaults ([maybe-default-LcExp?
                    (define variable? default-variable?)
                    (define lambda? default-lambda?)
                    (define app? default-app?)
                    (define LcExp? default-LcExp?)
                    (define (variable-symbol v) v)
                    (define lambda-symbol caadr)
                    (define lambda-body caddr)
                    (define app-operand cadr)
                    (define app-operator car)
                    ]))

#; (-> variable? LcExp? boolean?)
;; Returns #t if the variable occurs free in the LcExp, otherwise returns #f
(define (occurs-free? var expr)

  (define (occurs-free/sym? sym expr)
    (cond ((variable? expr) (eq? sym (variable-symbol expr)))
          ((lambda? expr)
           (and (not (eq? sym (lambda-symbol expr)))
                (occurs-free/sym? sym (caddr expr))))
          (else (ormap (lambda (e) (occurs-free/sym? sym e)) expr))))

  (occurs-free/sym? (variable-symbol var) expr))

#; (-> any/c boolean?)
;; Recognizes s-exp
(define (s-exp? v)
  (or (symbol? v) (s-list? v)))
(define (s-list? v)
  (and (list? v) (andmap s-exp? v)))

#; (-> symbol? symbol? (listof s-exp?) (listof s-exp?))
;; Replaces all occurrences of old in the s-exp list with instances of new
(define (subst new old slist)
  (map (lambda (s) (subst-s-exp new old s)) slist))
(define (subst-s-exp new old sexp)
  (cond ((eq? sexp old) new)
        ((symbol? sexp) sexp)
        (else (subst new old sexp))))

(module+ test
  (require rackunit (submod ".."))
  (check-true (LcExp? '(lambda (v) v)))
  (check-true (LcExp? 'v))
  (check-true (LcExp? '((lambda (v) v) (lambda (v) v))))
  (check-exn exn:fail:contract? (lambda () (LcExp? 1)))
  (check-true (occurs-free? 'v 'v))
  (check-false (occurs-free? 'v '(lambda (v) v)))
  (check-true (occurs-free? 'v '(lambda (x) v)))
  (check-true (occurs-free? 'v '(x (lambda (x) v))))
  (check-true (s-exp? '(s () (s))))
  (check-true (s-exp? 's))
  (check-false (s-exp? 1))
  (check-equal? (subst 'b 'a '(a () (a) c))
                '(b () (b) c)))
