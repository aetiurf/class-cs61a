(load 'lab12)

(define (halve x) (/ x 2))

(define (square x) (* x x))

(define halve-then-square (compose-all (list halve square)))

(halve-then-square 42)