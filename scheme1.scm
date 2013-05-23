;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements an interpreter for the proposition calculus using scheme.
;;;;
;;;;    Name:       scheme1.scm
;;;;    Purpose:    Implements an interpreter for the proposition calculus using scheme.
;;;;                Grammar details below:
;;;;
;;;;				< boolexp > ? #t | #f [boolean constants]
;;;;				< boolexp > ? variable [boolean variables]
;;;;				< boolexp > ? (and boolexp ... boolexp)
;;;;				< boolexp > ? (or boolexp ... boolexp)
;;;;				< boolexp > ? (not boolexp)
;;;;				< boolexp > ? (implies boolexp boolexp) [P => Q, where P is the first 
;;;;							   argument and Q is the second argument]
;;;;				< boolexp > ? (equiv boolexp boolexp) [P <=> Q, where P is the first 
;;;;							   argument and Q is the second argument] 
;;;;
;;;;    Authors:    Jon Boone
;;;;				Joshua Datko
;;;;				Paul DeMicco
;;;;				Joseph Heenan
;;;;
;;;;    Created:    05/21/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (beval expr env)
  (cond
    ((boolean? expr) expr)
    ((symbol? expr) (lookup expr env))
    ((conjuct? expr) (beval-and expr env))
    ((disjuct? expr) (beval-or expr env))
    ((negation? expr) (beval-not expr env))
    ((implication? expr) (beval-imply expr env))
    ((equivalence? expr) (beval-equiv expr env))
    (else (error "beval:illegal syntax"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Used in cond statement ;;;; 
;;;; in beval 			  	;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conjuct? expr) 
  (let ((function (car expr)))
    (eq? function 'and)))

(define (disjuct? expr) 
  (let ((function (car expr)))
    (eq? function 'or)))

(define (negation? expr) 
  (let ((function (car expr)))
    (eq? function 'not)))

(define (implication? expr) 
  (let ((function (car expr)))
    (eq? function 'imply)))
                  
(define (equivalence? expr) 
  (let ((function (car expr)))
    (eq? function 'equiv)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Evaluation Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup expr env)
  (let ((var-binding (assoc expr env)))
    (if (eq? var-binding #f)
        'error
        (let ((binding-val (car (cdr var-binding))))
          binding-val))))

(define (beval-and expr env)
  (let ((boolean-expr (cdr expr)))
    (if (null? boolean-expr)
        #t
        (let ((first-subexpr (car boolean-expr))
              (second-subexpr (cdr boolean-expr)))
          (and (beval first-subexpr env)
               (beval-and (cons 'and second-subexpr) env))))))

(define (beval-or expr env)
  (let ((boolean-expr (cdr expr)))
    (if (null? boolean-expr)
        #f
        (let ((first-subexpr (car boolean-expr))
              (second-subexpr (cdr boolean-expr)))
          (or (beval first-subexpr env)
              (beval-or (cons 'or second-subexpr) env))))))

(define (beval-not expr env)
  (let ((boolean-expr (car (cdr expr))))
    (not (beval boolean-expr env))))

(define (beval-imply expr env)
  (let* ((boolean-expr (cdr expr))
         (first-subexpr (car boolean-expr))
         (first-result (beval first-subexpr env))
         (second-subexpr (car (cdr boolean-expr)))
         (second-result (beval second-subexpr env)))
    (if (and first-result (not second-result))
        #f
        #t)))

(define (beval-equiv expr env)
  (let* ((boolean-expr (cdr expr))
         (first-subexpr (car boolean-expr))
         (first-result (beval first-subexpr env))
         (second-subexpr (car (cdr boolean-expr)))
         (second-result (beval second-subexpr env)))
    (if (and first-subexpr second-subexpr)
        #t
        (if (and (not first-subexpr) (not second-subexpr))
            #t
            #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Helper Functions   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (true? x) 
  (eq? x #t))

(define (false? x) 
  (not (eq? x #t)))

(define (andmap f xs)
  (cond ((null? xs) #t)
        ((f (car xs))
         (andmap f (cdr xs)))
        (else #f)))

(define (ormap f xs)
  (cond ((null? xs) #f)
        ((f (car xs)) #t)
        (else (ormap f (cdr xs)))))

(define (contains-only-true? list)
  (andmap true? list))

(define (contains-only-false? list)
  (andmap false? list))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Sample Input       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-test test-expression env)
  (pp test-expression)
  (write-string ";Value: ")
  (write-line (eval test-expression env)))

(let ((my-env (the-environment)))
  (run-test '(beval #t '()) my-env) ;Value: #t
  (run-test '(beval #f ()) my-env)  ;Value: #f
  (run-test '(beval 'p '((p #t))) my-env)  ;Value: #t  
  (run-test '(beval 'p '((a #t) (p #f))) my-env) ;Value: #f
  (run-test '(beval 'p '()) my-env) ;Value: error
  (run-test '(beval '(and #t p) '((p #t))) my-env) ;Value: #t  
  (run-test '(beval '(and p q r) '((p #t) (q #t) (r #t))) my-env) ;Value: #t  
  (run-test '(beval '(or p q r) '((p #t) (q #f) (r #f))) my-env) ;Value: #t
  (run-test '(beval '(not p) '((p #t))) my-env) ;Value: #f
  (run-test '(beval '(not p) '((p #f))) my-env) ;Value: #t
  (run-test '(beval '(not (or p q r)) '((p #f) (q #f) (r #f))) my-env) ;Value: #t
  (run-test '(beval '(imply p q) '((p #t) (q #f))) my-env) ;Value: #f  
  (run-test '(beval '(imply p q) '((p #t) (q #t))) my-env) ;Value: #t  
  (run-test '(beval '(imply p q) '((p #f) (q #f))) my-env) ;Value: #t
  (run-test '(beval '(imply p q) '((p #f) (q #t))) my-env) ;Value: #t
  (run-test '(beval '(equiv (imply p q) (or (not p) q)) '((p #t) (q #f))) my-env) ;Value: #t
  (run-test '(beval '(equiv (imply p q) (imply q p)) '((p #t) (q #f))) my-env) ;Value: t
  (run-test '(contains-only-true? '(#t #f #t #t #t)) my-env))

  
  