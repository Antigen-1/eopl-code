#lang racket/base
(require racket/contract)
(provide (contract-out (rename remq remove-first (-> symbol? (listof symbol?) (listof symbol?)))
                       (occurs-free? (-> symbol? LcExp? boolean?))
                       (subst (-> symbol? symbol? (listof s-exp?) (listof s-exp?))))
         LcExp? s-exp?
         (rename-out (length list-length)
                     (list-ref nth-element)))

#; (-> any/c exact-nonnegative-integer? boolean?)
;; Accepts a list l and a nonnegative integer n and find out whether l is a n-element list
(define (list-of-length? l n)
  (and (list? l) (list-of-length/unsafe? l n)))
(define (list-of-length/unsafe? l n)
  (or (and (zero? n) (null? l))
      (and (not (null? l)) (list-of-length/unsafe? (cdr l) (sub1 n)))))

#; (-> any/c boolean?)
;; Recognizes lambda-calculus expressions
(define (LcExp? v)
  (or
   (symbol? v)
   (and (list-of-length? v 3)
        (eq? (car v) 'lambda)
        (list-of-length? (cadr v) 1)
        (symbol? (caadr v))
        (LcExp? (caddr v)))
   (and (list-of-length? v 2)
        (andmap LcExp? v))))

#; (-> symbol? LcExp? boolean?)
;; Returns #t if the symbol occurs free in the LcExp, otherwise returns #f
(define (occurs-free? sym expr)
  (cond ((symbol? expr) (eq? sym expr))
        ((list-of-length? expr 3)
         (and (not (eq? sym (caadr expr)))
              (occurs-free? sym (caddr expr))))
        (else (ormap (lambda (e) (occurs-free? sym e)) expr))))

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
  (check-false (LcExp? 1))
  (check-true (occurs-free? 'v 'v))
  (check-false (occurs-free? 'v '(lambda (v) v)))
  (check-true (occurs-free? 'v '(lambda (x) v)))
  (check-true (occurs-free? 'v '(x (lambda (x) v))))
  (check-true (s-exp? '(s () (s))))
  (check-true (s-exp? 's))
  (check-false (s-exp? 1))
  (check-equal? (subst 'b 'a '(a () (a) c))
                '(b () (b) c)))
