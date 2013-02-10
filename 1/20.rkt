#lang r5rs

; Normal order evaluation:
(gcd 206 40)
(gcd 40 (remainder 206 40))
; call remainder once in if, then apply gcd
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; call remainder twice in if, then apply gcd
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; call remainder 4 times in if, then apply gcd
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; call remainder 7 times in if, then apply gcd
(remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
; call remainder 4 times while evaluating a, the return value of gcd
2

; total: called remainder 18 times in normal-order evaluation.



; Applicative order evaluation:
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2

; total: called remainder 4 times in applicative-order evaluation
