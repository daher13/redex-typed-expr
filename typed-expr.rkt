#lang racket

(require redex)

(define-language TypedExprL
  (v ::= true false natural)
  (e ::= v (or e e) (and e e) (+ e e) (if e e e) (< e e))
  (t ::= nat bool)
  (E ::= hole (or v E) (and v E) (+ v E) (+ E v) (if E e e) (< E e))
  )

(define ->e (reduction-relation TypedExprL
                                #:domain e
                                (--> (in-hole E (or e true)) (in-hole E true))
                                (--> (in-hole E (or e false)) (in-hole E e))
                                (--> (in-hole E (and e true)) (in-hole E e))
                                (--> (in-hole E (and e false)) (in-hole E false))
                                (--> (in-hole E (+ natural_1 natural_2)) (in-hole E ,(+ (term natural_1) (term natural_2))))
                                (--> (in-hole E (if true e_1 e_2)) (in-hole E e_1))
                                (--> (in-hole E (if false e_1 e_2)) (in-hole E e_2))
                                (--> (in-hole E (< natural_1 natural_2)) (in-hole E ,(if (< (term natural_1) (term natural_2)) (term true) (term false))))
                                ))


(define-judgment-form TypedExprL
  #:mode (types I O)
  #:contract (types e t)

  [
   ------------------------"T-true"
   (types true bool)
   ]
  
  [
   -----------------------"T-false"
   (types false bool)
   ]
  
  [
   ------------------------"T-natural"
   (types natural nat)
   ]

  [
   (types e_1 nat)
   (types e_2 nat)
   -----------------------"T-plus"
   (types (+ e_1 e_2) nat)
   ]

  [
   (types e_1 bool)
   (types e_2 bool)
   --------------------------"T-and"
   (types (and e_1 e_2) bool)
   ]

   [
   (types e_1 bool)
   (types e_2 bool)
   --------------------------"T-or"
   (types (or e_1 e_2) bool)
   ]

  [
   (types e_1 bool)
   (types e_2 t_e)
   (types e_3 t_e)
   -----------------------------"T-if"
   (types (if e_1 e_2 e_3) t_e)
   ]

  [
   -----------------------------"T-less"
   (types (< e_1 e_2) bool)
   ]
  )


(define (can-do-one-step? e)
  (= 1 (length (apply-reduction-relation ->e e))))


(define (types? e)
  (not (null? (judgment-holds (types ,e t)
                              t))))

(define (value? e)
  (not (null? (redex-match TypedExprL n e))))

(define (progress-holds? e)
  (if (types? e)
      (or (value? e)
          (can-do-one-step? e))
      #t))

(define (preservation-holds? e)
  (define types1 (judgment-holds (types ,e t) t))
  (unless (null? types1)
    (unless (= 1 (length types1)) (error 'preservation "multiple types! ~s" e)))
  (cond
    [(null? types1) #t]
    [else
     (for/and ([v (apply-reduction-relation* ->e e)])
       (equal? (judgment-holds (types ,v t) t)
               types1))]))

;; (can-do-one-step? (term (+ (+ 2 5) (+ 8 (+ 9 8)))))
;; (progress-holds? (term (+ (+ 1 3) (+ 3 4))))
;; (preservation-holds? (term (+ 1 2)))

;; (traces ->e (term (+ (+ 2 5) (+ 8 (+ 9 8)))))

;; (traces ->e (term (and (or false false) (or true true))))

;; (traces ->e (term (and true false)))

;; (traces ->e (term (if (and true false) (or true false) (+ 10 20))))

;; (traces ->e (term (< 4 5)))


(provide (all-defined-out))
