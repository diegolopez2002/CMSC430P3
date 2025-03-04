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
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))
