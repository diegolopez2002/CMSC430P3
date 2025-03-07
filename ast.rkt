#lang racket
(provide (all-defined-out))

;; Type definitions
;; Expr = (Lit Datum)
;;       | (Prim1 Op Expr)
;;       | (If Expr Expr Expr)
;;       | (Cond [Listof CondClause] Expr)
;;       | (Case Expr [Listof CaseClause] Expr)

;; Datum = Integer | Boolean

;; Op = 'add1 | 'sub1 | 'zero? | 'abs | '-' | 'not

(struct Lit (d) #:prefab)       ;; A literal (Integer or Boolean)
(struct Prim1 (p e) #:prefab)   ;; Unary operator applied to an expression
(struct If (e1 e2 e3) #:prefab) ;; If expression

;; CondClause = (Clause Expr Expr)


;; CaseClause = (Clause [Listof Datum] Expr)
(struct Cond (cs e) #:prefab)  
(struct Case (e cs el) #:prefab)  ;; Case expression with evaluated value, clauses, and else
(struct Clause (p b) #:prefab)  ;; Cond clause (condition, result)