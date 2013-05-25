;; f := 1;
;; f := 2;
;; f := 1

(define assign3 '((assign f 1)
		  (assign f 2)
		  (assign f 1)))

(meval assign3)
