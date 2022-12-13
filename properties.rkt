#lang racket

(require "./typed-expr.rkt"
         "./gen-expr.rkt"
         rackcheck
         rackunit
         redex/reduction-semantics)


;; generated expressions are well-formed

(define-property gen-wf
  ([in (generate)])
  (judgment-holds (types ,in t))
  (preservation-holds? in)
  (progress-holds? in)
  )

(check-property (make-config #:tests 100)
                gen-wf)
