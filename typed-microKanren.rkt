#lang typed/racket

;; Jason Hemann and Dan Friedman
;; microKanren, final implementation from paper

(define-type Variable (Immutable-Vector Natural))
(define-type Term (U Variable Boolean Symbol Null (Pair Term Term)))
(define-type Substitution (Listof (Pair Variable Term)))
(define-type State (Pair Substitution Natural))
(define-type Goal (-> State Stream))
(define-type Stream (U Mature-Stream Immature-Stream))
(define-type Empty-Stream Null)
(define-type Mature-Stream (U Empty-Stream (Pair State Stream)))
(define-type Immature-Stream (-> Stream))

(: var (-> Natural Variable))
(define (var c) (vector-immutable c))

(: var? (-> Term Boolean))
(define (var? x) (vector? x))

(: var=? (-> Variable Variable Boolean))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

(: walk (-> Term Substitution Term))
(define (walk u s)
  (let ((pr (and (var? u) (assf (lambda ([v : Variable]) (var=? (cast u Variable) v)) s))))
    (if pr (walk (cdr pr) s) u)))

(: ext-s (-> Variable Term Substitution (Option Substitution)))
(define (ext-s x v s) `((,x . ,v) . ,s))

(: == (-> Term Term Goal))
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(: unit (-> State Stream))
(define (unit s/c) (cons s/c mzero))

(: mzero Empty-Stream)
(define mzero '())

(: unify (-> Term Term Substitution (Option Substitution)))
(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
      ((and (var? u) (var? v) (var=? (cast u Variable) (cast v Variable))) s)
      ((var? u) (ext-s (cast u Variable) v s))
      ((var? v) (ext-s (cast v Variable) u s))
      ((and (pair? u) (pair? v))
       (let ((s (unify (car u) (car v) s)))
         (and s (unify (cdr u) (cdr v) s))))
      (else (and (eqv? u v) s)))))

(: call/fresh (-> (-> Variable Goal) Goal))
(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(: disj (-> Goal Goal Goal))
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))

(: conj (-> Goal Goal Goal))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(: mplus (-> Stream Stream Stream))
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car (cast $1 (Pair State Stream))) (mplus (cdr (cast $1 (Pair State Stream))) $2)))))

(: bind (-> Stream Goal Stream))
(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car (cast $ (Pair State Stream)))) (bind (cdr (cast $ (Pair State Stream))) g)))))

(module+ test
  (require typed/rackunit)

  (check-equal?
   (let (($ ((call/fresh (lambda (q) (== q 'a))) '(() . 0)))) (car (cast $ (Pair State Stream))))
   '(((#(0) . a)) . 1)))