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

(define has-ident?
  (lambda (identifier)
  (not (eq? (eval-ident identifier) none))))

(define eval-ident
  (lambda (identifier)
    (hash-table/get symboltable identifier none)))

(define identifier?
  (lambda (identifier)
    (string? identifier)))

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

(define minus-expr?
  (lambda (expr)
    (minus? (parse-operator expr))))

(define times?
  (lambda (expr)
    (eq? expr '*)))

(define times-expr?
  (lambda (expr)
    (times? (parse-operator expr))))

(define parse-operator 
	 (lambda(x) (car (cadr x))))

(define (eval-matharg arg)
  (cond
   ( (has-ident? (string arg)) (eval-ident (string arg)))
   ( (number? arg) arg)
   ( else #f)))

(define (eval-math expr op)
  (define parsedexpr (cadr expr))
  (let ((operator (parse-operator expr))
    (arg2   (eval-matharg (cadr parsedexpr)) )
    (arg3   (eval-matharg (caddr parsedexpr)) ))
  (op arg2 arg3)
  )
)

(define (math-expr? expr)
  (or (plus-expr? expr) (minus-expr? expr) (times-expr? expr)))

(define (eval-mathexpr expr)
  (cond
   ( (number? expr) expr)
   ( (plus-expr? expr) (eval-plus expr) )
   ( (minus-expr? expr) (eval-minus expr) )
   ( (times-expr? expr) (eval-times expr) )
   (else #f)
   )
)

(define (eval-plus expr)
  (eval-math expr +))

(define (eval-minus expr)
  (eval-math expr -))

(define (eval-times expr)
  (eval-math expr *))

(define eval-expr
  (lambda (expr)
    (cond
     ( (plus-expr? expr) #t)
     (else #f)
     )
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

