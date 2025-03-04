#lang racket
(provide compile-op1)
(require "ast.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define r9  'r9)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (seq (Add rax (value->bits 1)))]
    ['sub1 (seq (Sub rax (value->bits 1)))]
    ;; TODO: Handle abs, -, and not
    ['abs
     (let ((l1 (gensym))
           (l2 (gensym)))
       (seq (Cmp rax 0)
            (Jl l1)
            (Jmp l2)
            (Label l1)
            (Mov r9 0)
            (Sub r9 rax)
            (Mov rax r9)
            (Label l2)))]
    ['-
     (seq (Mov r9 0)
          (Sub r9 rax)
          (Mov rax r9))]
    ['not
     (seq (Cmp rax (value->bits #f))
          (Mov rax (value->bits #f))
          (Mov r9 (value->bits #t))
          (Cmove rax r9))]
    
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))
