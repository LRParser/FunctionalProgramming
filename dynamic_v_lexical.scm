;;Uncomment and load, but then you must type in the functions

;;;(define the-global-environment (setup-environment))
;;;(driver-loop)

;; This code behaves different depanding on scoping rules
(define x 3)

(define (foo)
  (begin (define x 5)
	 (add-two)))

(define (add-two)
  (+ 2 x))

(foo)

;; if scheme4.scm is loaded, foo returns 7 since the value of 5 shades the
;; gloabl variable and dynamic scoping is used

;; if ch4-mceval.scm is loaded, foo returns 5 since in add-two, x is bounded to
;; 3, which is defined in the global environment.  Lexically, this is the only
;; x that add-two can see.
