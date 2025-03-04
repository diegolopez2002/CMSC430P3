#lang racket
(provide interp-prim1)

;; Op Value -> Value
(define (interp-prim1 op v)
  (match op
    ['add1  (add1 v)]        ; Handle 'add1 operation
    ['sub1  (sub1 v)]        ; Handle 'sub1 operation
    ['abs (abs v)]
    ['- (- v)]
    ['not (not v)]
    ;;TODO : Handle abs, -, and not
    ['zero? (zero? v)]))