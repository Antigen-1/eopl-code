#lang racket/base
(require (for-syntax racket/base racket/list)
         syntax/parse/define
         racket/list racket/function racket/contract)
(provide define-datatype cases)

;; Data Abstraction
;; -------------------------------------------------------------------
;; <Variant> data abstraction
#; (cons/c
    symbol?
    (cons
     exact-nonnegative-integer?
     (cons/c
      struct-constructor-procedure?
      (cons/c struct-predicate-procedure?
              (listof (-> <Instance> any))))))
;; Generate a variant using its name and the number of its fields
#; (-> symbol? exact-nonnegative-integer?
       <Variant>)
(define (make-variant name field-num)
  (call-with-values (lambda () (make-struct-type name #f field-num 0))
                    (lambda (type construct pred access mutate)
                      (append
                       (list name field-num construct pred)
                       (map (lambda (n) (lambda (s) (access s n)))
                            (range 0 field-num))))))
;; Variant Observers
#; (-> <Variant> symbol?)
(define (variant-name v) (car v))
#; (-> <Variant> exact-nonnegative-integer?)
(define (variant-field-number v) (cadr v))
#; (-> <Variant> struct-constructor-procedure?)
(define (variant-construct v) (caddr v))
#; (-> <Variant> struct-predicate-procedure?)
(define (variant-predicate v) (cadddr v))
#; (-> <Variant> (listof (-> <Instance> any)))
(define (variant-accessors v) (cddddr v))

;; A syntax transformer used to wrap constructors with contracts
(define-syntax-rule (wrap-constructor name construct pred ...)
  (let ()
    (define/contract name
      (-> pred ... any)
      construct)
    name))

;; A structure that represents the datatype
(struct type (pred variants) #:constructor-name make-type)

;; A syntax transformer used to define the datatype
(begin-for-syntax
  (define (check-variant-names variants)
    (cond ((check-duplicates (syntax->datum variants) eq?)
            =>
            (lambda (sym) (raise (make-exn:fail:syntax
                                  (format "Duplicate variant name: ~a." sym)
                                  (current-continuation-marks))))))))
(define-syntax-parser define-datatype
  ((_ name:id type-pred:id (variant:id (field:id field-pred) ...) ...)
   #:declare field-pred (expr/c #'(-> any/c boolean?) #:name "Field Predicate")
   (let ((introduce (lambda (stx) ((compose cdr syntax-e syntax-local-introduce) #`(name . #,stx)))))
     (check-variant-names #'(variant ...))
     (with-syntax (((field-num ...) (map length (syntax->datum #'((field ...) ...))))
                   ((variant-accessor ...)
                    (map
                     introduce
                     (append*
                      (map
                       (lambda (var fields)
                         (map
                          (lambda (f)
                            (string->symbol
                             (string-append
                              (symbol->string var)
                              "-"
                              (symbol->string f))))
                          fields))
                       (syntax->datum #'(variant ...))
                       (syntax->datum #'((field ...) ...)))))))
       #'(define-values (name type-pred variant ... variant-accessor ...)
           (let* ((variants (map make-variant '(variant ...) (list field-num ...)))
                  (type-pred (apply disjoin (map variant-predicate variants))))
             (apply values
                    (make-type type-pred variants)
                    type-pred
                    (append*
                     (map
                      (lambda (v wrap) (wrap (variant-construct v)))
                      variants
                      (list (lambda (proc)
                              (wrap-constructor
                               variant
                               proc
                               field-pred ...))
                            ...))
                     (map variant-accessors variants)))))))))

(module+ test
  (require rackunit)
  (define-datatype my-list my-list?
    (empty)
    (pair (car (const #t)) (cdr my-list?)))
  (check-exn exn:fail:syntax?
             (lambda () (expand '(define-datatype mylist mylist (empty) (empty)))))
  (check-true (my-list? (empty)))
  (check-true (my-list? (pair #t (empty))))
  (check-false (my-list? null))
  (check-exn exn:fail:contract? (lambda () (pair "" null)))
  (check-true (pair-car (pair #t (empty))))
  (check-exn exn:fail:contract? (lambda () (pair-car (empty))))
  )

;; A syntax transformer used to match an instance of a datatype
(begin-for-syntax
  (define-splicing-syntax-class else-branch
    #:literals (else)
    #:description "The Else Branch"
    (pattern (~seq (else body ...))
             #:with else-bodies #'(body ...))
    (pattern (~seq)
             #:with else-bodies #'((void)))))
(define-syntax-parser cases
  ((_ type-value expr ((variant:id field:id ...) body ...) ... eb:else-branch)
   #:declare type-value (expr/c #'type? #:name "Type Structure")
   (let ()
     (check-variant-names #'(variant ...))
     (with-syntax ((((name field-num) ...) (map (lambda (name fields) (list name (length fields))) (syntax->datum #'(variant ...)) (syntax->datum #'((field ...) ...)))))
       #`(let ((variants (type-variants type-value)))
           (define/contract result (type-pred type-value) expr)
           (cond ((let ((v (findf (lambda (v) (eq? (variant-name v) 'name)) variants)))
                    (cond
                      ((not v) (raise (make-exn:fail:contract (format "Variant not found: ~a." 'name)
                                                              (current-continuation-marks))))
                      ((not (= field-num (variant-field-number v)))
                       (raise (make-exn:fail:contract (format "Wrong field number in ~a." '(variant field ...))
                                                      (current-continuation-marks))))
                      ((and ((variant-predicate v) result) v))
                      (else #f)))
                  =>
                  (lambda (v)
                    (let-values (((field ...) (apply values (map (lambda (p) (p result)) (variant-accessors v)))))
                      body ...)))
                 ...
                 (else #,@#'eb.else-bodies)))))))

(module+ test
  (check-true (void? (cases my-list (pair 1 (empty)))))
  (check-true (cases my-list (pair 1 (empty))
                     (else #t)))
  (check-true (cases my-list (empty)
                     ((empty) #t)))
  (cases my-list (pair 1 (empty))
         ((pair car cdr)
          (check-true (= car 1))
          (check-true (my-list? cdr))))
  (check-true (cases my-list (pair 1 (empty))
                     ((empty) #f)
                     (else #t)))
  (check-exn exn:fail:contract? (lambda () (cases my-list null)))
  (check-exn exn:fail:contract? (lambda () (cases + 1)))
  (check-exn exn:fail:contract? (lambda () (cases my-list (empty)
                                                  ((pair) #t))))
  (check-exn exn:fail:contract? (lambda () (cases my-list (empty)
                                                  ((_) #t))))
  (check-exn exn:fail:syntax? (lambda () (expand '(begin (define-datatype mylist mylist? (empty) (pair (car (lambda (_) #t)) (cdr mylist?)))
                                                         (cases mylist (empty)
                                                                ((empty) (void))
                                                                ((empty) (void)))))))
  )
;; -------------------------------------------------------------------
