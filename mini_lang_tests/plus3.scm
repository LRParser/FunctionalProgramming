;;x := (1 + 5 + 8);
;;y := 2 + 5

(define plus3 '((assign x (+ (+ 1 5) 8))
	       (assign y (+ 2 5))))

(meval plus3)
