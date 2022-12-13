#lang racket
(require redex)

(define-language TyExp
  ;; syntax
  (e true
     false
     zero
     (suc e)
     (pred e)
     (iszero e)
     (ift e e e))
  ;; types
  (t nat
     bool)
  ;; evaluation contexts
  (E hole
     (suc E)
     (pred E)
     (iszero E)
     (ift E e e))
  ;; values
  (bv true
      false)
  (nv zero
      (suc nv))
  (v  bv
      nv))

;; testing if an expression is a value

(define (nv? e)
  (redex-match TyExp nv e))

(define (value? e)
  (redex-match TyExp v e))

;; small step semantics

(define eval-tyexp
  (reduction-relation
   TyExp
   ;; rules for pred
   (--> (in-hole E (pred zero))
        (in-hole E zero)
        "pred-zero")
   (--> (in-hole E (pred (suc v)))
        (in-hole E v)
        (side-condition (nv? (term v)))
        "pred-suc")
   ;; rules for iszero
   (--> (in-hole E (iszero zero))
        (in-hole E true)
        "iszero-zero")
   (--> (in-hole E (iszero (suc v)))
        (in-hole E false)
        (side-condition (nv? (term v)))
        "iszero-suc")
   ;; rules for ift
   (--> (in-hole E (ift true e_1 e_2))
        (in-hole E e_1)
        "if-true")
   (--> (in-hole E (ift false e_1 e_2))
        (in-hole E e_2)
        "if-false")))

;; checking if an expression can step

(define (can-do-one-step? e)
  (= 1 (length (apply-reduction-relation
                eval-tyexp e))))

;; type system

(define-judgment-form TyExp
  #:mode (types I O)
  #:contract (types e t)

  [
   ------------------------"T-zero"
      (types zero nat) 
  ]

  [
   -------------------------"T-false"
      (types false bool)
  ]

  [
   -------------------------"T-true"
      (types true bool)
  ]

  [
         (types e nat)
   --------------------------"T-suc"
       (types (suc e) nat)
  ]

  [
         (types e nat)
   --------------------------"T-pred"
       (types (pred e) nat)
  ]

  [
         (types e nat)
   --------------------------"T-iszero"
       (types (iszero e) bool)
  ]

  [
         (types e_1 bool)
         (types e_2 t_e)
         (types e_3 t_e)
   -----------------------------------"T-if"
        (types (ift e_1 e_2 e_3) t_e)
  ])


;; well-typedness predicate

(define (types? e)
  (not (null? (judgment-holds (types ,e t)
                              t))))


;; progress property

(define (progress-holds? e)
  (if (types? e)
      (or (value? e)
          (can-do-one-step? e))
      #t))

;; preservation property

(define (preservation-holds? e)
  (define types1 (judgment-holds (types ,e t) t))
  (unless (null? types1)
    (unless (= 1 (length types1)) (error 'preservation "multiple types! ~s" e)))
  (cond
    [(null? types1) #t]
    [else
     (for/and ([v (apply-reduction-relation* eval-tyexp e)])
       (equal? (judgment-holds (types ,v t) t)
               types1))]))

;; test cases

(define (progress)
  (let ([c (make-coverage eval-tyexp)])
    (parameterize ([relation-coverage (list c)])
        (redex-check TyExp 
               #:satisfying (types e t) 
               (progress-holds? (term e)))
    (covered-cases c))))

(define (preservation)
  (let ([c (make-coverage eval-tyexp)])
    (parameterize ([relation-coverage (list c)])
        (redex-check TyExp 
               #:satisfying (types e t) 
               (preservation-holds? (term e)))
    (covered-cases c))))
