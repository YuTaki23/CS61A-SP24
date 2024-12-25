(define (over-or-under num1 num2)
  (cond
    ((< num1 num2) -1)
    ((> num1 num2) 1)
    (else 0)
  )
)

(define (make-adder num)
  (define (add_helper x) (+ x num))
  add_helper
)

(define (composed f g)
  (define (composed_helper x) (f(g x)))
  composed_helper
)

(define (repeat f n)
  (define (repeat_helper x)
    (if (= n 1) (f x)
        ((repeat f (- n 1)) (f x))
    )
  )
  repeat_helper
)

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b)
  (if (zero? (modulo a b))
    (min a b)
    (gcd (min a b) (modulo (max a b) (min a b)))
  )
)
