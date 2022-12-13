#lang racket

(require rackcheck)

(require "./typed-expr.rkt")
(require redex)

(define (cgen:const t)
  (match t
    ['bool (gen:choice (gen:const 'true) (gen:const 'false))]
    ['nat (gen:integer-in 1 100)]))

(define (cgen:type)
  (gen:choice (gen:const 'nat) (gen:const 'bool)))

(define (cgen:expr t n)
  (match (cons t n)
    [(cons 'nat 0) (cgen:const 'nat)]
    [(cons 'bool 0) (cgen:const 'bool)]
    [(cons 'nat n) (gen:choice (cgen:const 'nat)
                             (gen:let ([e1 (cgen:expr 'nat (sub1 n))]
                                       [e2 (cgen:expr 'nat (sub1 n))])
                                      (gen:const (list '+ e1 e2)))
                             (gen:let ([ec (cgen:expr 'bool (sub1 n))]
                                       [e1 (cgen:expr 'nat (sub1 n))]
                                       [e2 (cgen:expr 'nat (sub1 n))])
                                      (gen:const (list 'if ec e1 e2)))
                             )]
    [(cons 'bool n) (gen:choice (cgen:const 'bool)
                              (gen:let ([e1 (cgen:expr 'bool (sub1 n))]
                                        [e2 (cgen:expr 'bool (sub1 n))]
                                        [op (gen:choice (gen:const 'or) (gen:const 'and))])
                                       (gen:const (list op e1 e2)))
                              (gen:let ([ec (cgen:expr 'bool (sub1 n))]
                                        [e1 (cgen:expr 'bool (sub1 n))]
                                        [e2 (cgen:expr 'bool (sub1 n))])
                                       (gen:const (list 'if ec e1 e2)))
                              (gen:let ([e1 (cgen:expr 'nat (sub1 n))]
                                        [e2 (cgen:expr 'nat (sub1 n))])
                                       (gen:const (list '< e1 e2)))
                              )]))

;; (define test-boolean (term ,(car (sample (cgen:expr 'bool 5) 1))))
;; (define test-plus (term ,(car (sample (cgen:plus-expr 10) 1))))
;; (define test-if (term ,(car (sample (cgen:if-expr) 1))))

;; (traces ->e test-boolean)
;; (traces ->e test-plus)
;; (traces ->e test-if)

;; (preservation-holds? test-boolean)
;; (preservation-holds? test-plus)

(define (test-preservation l)
  (map (lambda (i)
         (cons i (preservation-holds? (term ,i)))
         ) l))

(define (test-progress l)
  (map (lambda (i)
         (cons i (progress-holds? (term ,i)))
         ) l))

(define (generate)
  (gen:bind (cgen:type) (lambda (t)
                          (gen:bind (gen:integer-in 3 5) (lambda (h)
                                                           (cgen:expr t h))))))


;; (test-preservation (sample (cgen:expr 'nat 5) 100))
;; (test-progress  (sample (cgen:expr 'nat 5) 100))

(provide (all-defined-out))
