(define fact1
  '((assign n (- 0 5))
    (if n ((assign i n)) ((assign i (- 0 n))))
    (assign fact 1)
    (while i ((assign fact (* fact i))
	      (assign i(-i 1))))))

(meval fact1)
