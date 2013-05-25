;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements an interpreter for the mini-language using scheme.
;;;;
;;;;    Name:       scheme5.scm
;;;;    Purpose:    Implements an interpreter for the mini-language using scheme.
;;;;                Grammar details below:
;;;;
;;;;                    < prog > ? (stmtlist)
;;;;                < stmtlist > ? (stmt stmtlist) | (stmt)
;;;;                    < stmt > ? assign_stmt | if_stmt | while_stmt
;;;;             < assign_stmt > ? (assign id expr)
;;;;                 < if_stmt > ? (if expr stmtlist stmtlist)
;;;;              < while_stmt > ? (while expr stmtlist)
;;;;                    < expr > ? (+ expr term) | (- expr term) | term
;;;;                    < term > ? (* term factor) | factor
;;;;                  < factor > ? ( expr ) | NUMBER | IDENT
;;;;
;;;;
;;;;    Authors:    Jon Boone
;;;;		    Joshua Datko
;;;;		    Paul DeMicco
;;;;		    Joseph Heenan
;;;;
;;;;    Created:    05/25/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; program evaluator - provided by Dr Johnson
(define (meval prog)
  (let ((env (initial-environment)))
    (if (stmtlist? prog)
        (begin
          (pp (hash-table->alist (eval-stmtlist prog env)))
          'ok)
        (error "illegal program"))))

;;; stmtlist evaluator - provided by Dr Johnson
(define (eval-stmtlist stmtlist env)
  (if (null? stmtlist)
      env
      (eval-stmtlist (cdr stmtlist) (eval-stmt (car stmtlist) env))))

;;; stmt evaluator - provided by Dr Johnson
(define (eval-stmt stmt env)
  (cond
   ((assign-stmt? stmt) (eval-assign stmt env))
   ((if-stmt? stmt) (eval-if stmt env))
   ((while-stmt? stmt) (eval-while stmt env))))


;;; assignstmt evaluator - provided by Dr Johnson
(define (eval-assign stmt env)
  (let ((var (cadr stmt))
        (expr (caddr stmt)))
    (insert-binding var (eval-expr expr env) env)))

;;; ifstmt evaluator - provided by Dr Johnson
(define (eval-if stmt env)
  (let ((expr (cadr stmt))
        (S1 (caddr stmt))
        (S2 (cadddr stmt)))
    (if (eval-expr expr env)
        (eval-stmtlist S1 env)
        (eval-stmtlist S2 env))))

;;; whilestmt evaluator - provided by Dr Johnson
(define (eval-while stmt env)
  (define (loop expr S env)
    (if (eval-expr expr env)
        (loop expr S (eval-stmtlist S env))
        env))
  (let ((expr (cadr stmt))
        (S (caddr stmt)))
    (loop expr S env)))


;;; verify that the stmtlist is valid
(define (stmtlist? stmts)
  (if (list? stmts)
      (if (null? stmts)
          #f ; empty-list - not valid
          (verify-stmts stmts))
      #f)) ; not list - not valid

;;; verify that the individual stmts are valid
(define (verify-stmts stmts)
  (if (null? stmts)
      #t
      (let ((first-stmt (car stmts))
            (rest (cdr stmts)))
        (and (or (assign-stmt? first-stmt)
                 (if-stmt? first-stmt)
                 (while-stmt? first-stmt))
             (verify-stmts rest)))))

;;; check whether the stmt is an assign stmt
(define (assign-stmt? stmt)
  (if (eq? (car stmt) 'assign)
      #t
      #f))

;;; check whether the stmt is an if stmt
(define (if-stmt? stmt)
  (if (eq? (car stmt) 'if)
      #t
      #f))

;;; check whether the stmt is a while stmt
(define (while-stmt? stmt)
  (if (eq? (car stmt) 'while)
      #t
      #f))

;; Environment functions
;;; set up the initial environment
(define (initial-environment)
  (make-equal-hash-table 100))

;;;Insert the binding into the current envirnoment and return the updated hash
;;;table
(define (insert-binding var val env)
  (begin (hash-table/put! env var val)
	 env))

;;; Lookup the binding and return the value, returns "none" if not found
(define (lookup-binding var env)
  (let ((binding (hash-table/get env var 'none)))
    (if (eq? binding 'none)
        (error "unbound variable")
        binding)))

;;; expr evaluator
(define (eval-expr expr env)
  (if (list? expr)
      (cond
       ((plus-expr? expr) (eval-plus expr env))
       ((minus-expr? expr) (eval-minus expr env))
       (else (eval-term expr env)))
      (eval-term expr env)))

;;; term evaluator
(define (eval-term expr env)
  (if (list? expr)
      (if (times-expr? expr)
          (eval-times expr env)
          (eval-factor expr env))
      (eval-factor expr env)))

;;; factor evaluator
(define (eval-factor expr env)
  (cond
   ((list? expr) (let ((factor (car expr)))
                   (eval-factor factor env)))
   ((number? expr) expr)
   ((ident? expr) (eval-ident expr env))
   (else #f)))


;;; ident predicate
(define (ident? expr)
  (if (symbol? expr)
      #t
      #f))

;;; ident evaluator
(define (eval-ident expr env)
  (lookup-binding expr env))

;;; plus-expr? predicate
(define (plus-expr? expr)
  (if (eq? (car expr) '+)
      #t
      #f))

;;; minus-expr? predicate
(define (minus-expr? expr)
  (if (eq? (car expr) '-)
      #t
      #f))

;;; times-expr? predicate
(define (times-expr? expr)
  (if (eq? (car expr) '*)
      #t
      #f))

;;; plus-expr evaluator
(define (eval-plus expr env)
  (let ((first-arg (eval-expr (cadr expr) env))
        (second-arg (eval-term (caddr expr) env)))
    (+ first-arg second-arg)))

;;; minus-expr evaluator
(define (eval-minus expr env)
  (let ((first-arg (eval-expr (cadr expr) env))
        (second-arg (eval-term (caddr expr) env)))
    (- first-arg second-arg)))

;;; times-expr evaluator
(define (eval-times expr env)
  (let ((first-arg (eval-term (cadr expr) env))
        (second-arg (eval-factor (caddr expr) env)))
    (* first-arg second-arg)))

;; Display a nice done message when loaded
'MINI-LANGUAGE-INTERPRETER-LOADED
