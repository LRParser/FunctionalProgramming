;; Grammar, for reference
;; 1. prog -> stmt-list
;; 2. stmt-list -> (stmt-seq)
;; 3. stmt-seq -> stmt | stmt stmt-seq
;; 4. stmt -> assign-stmt | if-stmt | while-stmt
;; 5. assign-stmt -> (assign identifier expr)
;; 6. if-stmt -> (if expr stmt-list stmt-list)
;; 7. while-stmt -> (while expr stmt-list)
;; 8. expr -> integer | identifier | (+ expr expr) | (- expr expr) | (* expr expr)

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

(define assign-stmt?
  (lambda (stmt)
    (eq? (car (cadr stmt)) 'assign)))

(define eval-assign
  (lambda (assignstmt)
    (define ident (cadr (cadr assignstmt)))
    (define val (car (cdr (cdr (cadr assignstmt)))))
    (assign (string ident) val)))

; END Assign

; BEGIN MATH


(define parse-operator
	 (lambda(x) (car (car x))))

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


(define ( eval-times expr)
 (let ((operator (car expr))
    (arg1 (car expr))
    (arg2 (cadr expr))
    (arg3 (caddr expr)))
  (* arg2 arg3)
  )
)


(define (eval-matharg arg)
  (cond
   ( (has-ident? (string arg)) (eval-ident (string arg)))
   ( (number? arg) arg)
   ( else #f)))

(define (eval-math expr op)
  (define parsedexpr (car expr))
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
   ( (minus-expr? expr) (eval-minusnew expr) )
   ( (times-expr? expr) (eval-times expr) )
   (else #f)
   )
)

(define (eval-plus expr)
  (eval-math expr +))

(define (eval-minus expr)
  (eval-math expr -))

(define eval-minusnew
  (lambda (arg1 arg2)
    (- (eval-matharg arg1) (eval-matharg arg2))))

(define (eval-times expr)
  (eval-math expr *))

(define eval-expr
  (lambda (expr)
    (cond
     ( (math-expr? expr) (eval-mathexpr expr))
     (else #f)
     )
    )
)


; END Math

; Begin Stmt

(define eval-stmt
  (lambda (stmt)
    (cond
     ( (while-stmt? stmt) (eval-while stmt))
     ( (if-stmt? stmt) (eval-if stmt))
     ( (assign-stmt? stmt) (eval-assign stmt))
     ( else #f))))

; Begin StmtList

(define make-stmtseq
  (lambda (stmt stmtseq)
    (vector stmt stmtseq)))

(define car-stmtlist
  (lambda (stmtlist)
    (cadr stmtlist)))

(define eval-stmtlist
  (lambda (sl)
    (vector-map eval-stmt sl)))

; End StmtList

; Begin While

(define while?
  (lambda (expr)
    (eq? (car (cadr expr)) 'while)))

(define while-expr?
  (lambda (expr)
    (while? (parse-operator expr))))

(define while-cond
  (lambda (expr)
    (cadr (cadr expr))))  ; Get the condition associated with the while

(define while-stmtlist
  (lambda (expr)
    (car (cdr (cdr (cadr w)))))) ; Get the stmtlist associated with the while

(define eval-while
  (lambda (expr)
    (define cond (while-cond expr))
    (define sl (while-stmtlist expr))
    (unless? (eq? 0 (eval-math cond))
	     (eval-stmtlist sl))))

(define (while cond . args)
  ( (unless? (eq? cond 0)
	     (for-each
	      (lambda(x) (eval-stmt x)) args))))

; Begin If

(define eval-if
  (lambda (ifcond truelist falselist)
    (cond
     ( (eq? 0 (eval-matharg ifcond)) (eval-stmtlist truelist))
     ( else (eval-stmtlist falselist)))))

; End If

; Begin while tests

(define i (string 'i))
(assign i 5)

(define f '( (+ 1 2) ))
(define g '( (+ 3 5) ))
(define assignstmt1 '( (assign j 10)))
(define h '( (+ i j)))
(define assignstmt2 '( (assign j 11)))
(define stmtlist '( ((assign j 10) (assign j 11))))
(define ifstmt '( if f ((assign j 10) (assign j 11))))
(plus-expr? f)
(eval-mathexpr f)
(eval-assign assignstmt1)
(eval-mathexpr h)
(eval-expr h)

; Begin PROF Example

(define assignprof '((asign n (0 - 5))))

; Example usagee:
;48 error> (define f2 (read))
;'(+ 1 2)
; (plus-expr? f2)
