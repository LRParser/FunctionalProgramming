;; i:=5;
;; j := 0;
;; while i do
;; i := i - 1;
;; j := j + 1
;; od

(define while1
  '((assign i 5)
    (assign j 0)
    (while (i)
	   (begin (assign i (- i 1))
		  (assign j (+ j 1))))))

(meval while1)
