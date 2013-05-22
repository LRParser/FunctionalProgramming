(define (string-input)
   (display "Enter expression: ")
   (define readtext (string (read)))
   (display (string-append "Got input: " readtext))
   (newline)
   readtext)

; Begin ASSIGN

(define symboltable (make-string-hash-table 100))

(define assign
  (lambda (identifier expr)
    (hash-table/put! symboltable identifier expr)))

(define none (string 'none))

(define null '())

(define eval-ident
  (lambda (identifier)
    (hash-table/get symboltable identifier none)))

; END Assign 

; BEGIN MATH

(define plus?
  (lambda (expr)
    (eq? expr '+)))

(define plus-expr?
	  (lambda (expr) 
	    (plus? (parse-operator expr))))

(define minus?
  (lambda (expr)
    (eq? expr '-)))

(define times?
  (lambda (expr)
    (eq? expr '*)))

(define parse-operator 
	 (lambda(x) (car (cadr x))))

(define (eval-plus expr)
  (define parsedexpr (cadr expr))
  (let ((operator (parse-operator expr))
    (arg2 (cadr parsedexpr))
    (arg3 (caddr parsedexpr)))
  (+ arg2 arg3)
  )
)

(define eval-expr
  (lambda (expr)
    (cond
     ( (plus-expr? expr) #t)
     (else #f)
     )
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
; END Math

(define make-stmtseq
  (lambda (stmt stmtseq)
    (cons stmt stmtseq)))



(define ( eval-times expr)
 (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (* arg2 arg3)
  )
)




(define read-input
  (lambda (expr)
    (delay expr)))

; Example usagee:
;48 error> (define f2 (read))
;'(+ 1 2)
; (plus-expr? f2)

