(define (get-input)
   (display "Enter expression: ") 
   (define readtext (string (read)))
   (display (string-append "Got input: " readtext))
   (newline)
   readtext)

(define plus?
  (lambda (expr)
    (eq? expr #\+)))

(define minus?
  (lambda (expr)
    (eq? expr #\-)))

; Example usage: (eval-plus '(+ 1 2))
(define (eval-plus expr)
  (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (+ arg2 arg3)
  )
)

; Example usage: (eval-minus '(- 1 2))
(define ( eval-minus expr)
 (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (- arg2 arg3)
  )
)

