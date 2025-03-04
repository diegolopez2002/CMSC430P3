#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? datum?)          (Lit s)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'cond (list 'else e)) (Cond '() (parse e))]
    [(list 'cond (list p e) rest ... (list 'else exp))
     (Cond (flatten (list (Clause (parse p) (parse e)) (parse-clause rest))) (parse exp))]
    [(list 'case e (list 'else el)) (Case (parse e) '() (parse el))]
    [(list 'case e (list datums result) rest ... (list 'else el))
     (Case (parse e) (flatten (list (Clause datums (parse result)) (parse-datum rest))) (parse el))]
    [_ (error "Parse error")]))

(define (parse-clause cs)
  (match cs
    ['() '()]
    [(list (list p e)) (list (Clause (parse p) (parse e)))]
    [(list (list p e) rest ...) (flatten (list (Clause (parse p) (parse e)) (parse-clause rest)))]))

(define (parse-datum cs)
  (match cs
     ['() '()]
     [(list (list datums exp)) (list (Clause datums (parse exp)))]
     [(list (list datums exp) rest ...) (flatten (list (Clause datums (parse exp)) (parse-datum rest)))]))



;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)))

(define (op1? x)
  (memq x '(add1
            sub1
            zero?
            abs
            -
            not)))
