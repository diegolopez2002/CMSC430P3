#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Lit d) d]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(Cond cs e) (interp-clause cs e)]
    [(Case e cs el) (interp-datum e cs el)]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))



(define (interp-clause cs e)
  (match cs
    ['() (interp e)]
    [(list (Clause p exp))
     (if (interp p) (interp exp) (interp e))]
    [(list (Clause p exp) rest ...)
     (if (interp p) (interp exp) (interp-clause rest e))]))

(define (interp-datum e cs el)
  (match cs
     ['() (interp el)]
     [(list (Clause lst exp))
      (if (member (interp e) lst) (interp exp) (interp el))]
     [(list (Clause lst exp) rest ...)
      (if (member (interp e) lst) (interp exp) (interp-datum e rest el))]))
    

