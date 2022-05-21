
(define-macro (def func args body)
    `(define ,func (lambda ,args ,body))
)


(define (map-stream f s)
    (if (null? s)
    	nil
    	(cons-stream (f (car s)) (map-stream f (cdr-stream s)))))

; !!!!!!!!
(define all-three-multiples
    (map-stream (lambda (x) (+ 3 x))
        (cons-stream 0 all-three-multiples)
    )
)


(define (compose-all funcs)
    (lambda (x)
        (define (compose funcs x)
            (if (null? funcs)
                x
                (compose (cdr funcs) ((car funcs) x))
            )
        )
        (compose funcs x)
    )
)


(define (partial-sums stream)
    (define (helper x stream)
        (if (null? stream) nil
            (cons-stream (+ x (car stream)) 
                (helper (+ x (car stream)) (cdr-stream stream))
            )
        )
    )
    (helper 0 stream)
)

