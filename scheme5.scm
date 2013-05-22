(define (string-input)
   (display "Enter expression: ")
   (define readtext (string (read)))
   (display (string-append "Got input: " readtext))
   (newline)
   readtext)

(define symboltable (make-string-hash-table 100))

(define assign
  (lambda (identifier expr)
    (hash-table/put! symboltable identifier expr)))

(define none (string 'none))

(define eval-ident
  (lambda (identifier)
    (hash-table/get symboltable identifier none)))

(define plus?
  (lambda (expr)
    (eq? expr '+)))

(define minus?
  (lambda (expr)
    (eq? expr '-)))

(define times?
  (lambda (expr)
    (eq? expr '*)))

(define parse-operator 
	 (lambda(x) (car (cadr x))))

(define (eval-plus expr)
  (define parsedexpr (car (cadr expr)))
  (let ((operator (car expr))
    (arg1 (car parsedexpr))
    (arg2 (cadr parsedexpr))
    (arg3 (caddr parsedexpr)))
  (+ arg2 arg3)
  )
)

(define ( eval-minus expr)
 (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (- arg2 arg3)
  )
)

(define ( eval-times expr)
 (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (* arg2 arg3)
  )
)



(define plus-expr?
	  (lambda (expr) 
	    (plus? (parse-expr expr))))

; Example usagee:
;48 error> (define f2 (read))
;'(+ 1 2)
; (plus-expr? f2)

