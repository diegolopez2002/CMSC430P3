#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rbx 'rbx)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d)         (compile-value d)]
    [(Prim1 p e)     (compile-prim1 p e)]
    [(Cond cs e)     (compile-clause cs e)]  ;; Handle cond
    [(Case e cs el)  (compile-case e cs el)]  ;; Handle case
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))


(define (compile-clause cs e)
  (match cs
    ['() (compile-e e)]
    [(list (Clause p exp))
     (compile-if p exp e)]
    [(list (Clause p exp) rest ...)
     (compile-if p exp (Cond rest e))]))


(define (compile-case e cs el)
  (let ((done (gensym 'done))
        (else (gensym 'else)))
    (seq (compile-e e)
         (compile-datum cs else done (gensym 'case))
         (Label else)
         (compile-e el)
         (Label done))))


(define (compile-datum cs else done pcase)
  (let ((case (gensym 'case)))  ; case symbol is generated but not used
    (match cs
      ['() (seq (Jmp else))]  ; Jump to else if no clauses are left
      [(list (Clause '() _) rest ...)
       (compile-datum rest else done case)]  ; Continue with remaining clauses
      [(list (Clause (list a) exp) rest ...)
       (seq (Mov rbx (value->bits a))        ; Move first case value into rbx
            (Cmp rax rbx)                   ; Compare rax and rbx
            (Je pcase)                      ; Jump to pcase if equal
            (compile-datum rest else done case)
            (Label pcase)                   ; Label for matching case
            (compile-e exp)                 ; Compile expression for this case
            (Jmp done))]                    ; Jump to done after case is compiled
      [(list (Clause (list a datums ...) exp) rest ...)
       (seq (Mov rbx (value->bits a))        ; Move first value to rbx
            (Cmp rax rbx)                   ; Compare rax and rbx
            (Jmp pcase)                     ; Jump if matching
            (compile-datum (append (list (Clause datums exp)) rest) else done pcase))]))) ; Recursion for multiple datums