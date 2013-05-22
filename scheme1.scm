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

;;;; Just a place holder at the moment
(define (implication? expr) 
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (equivalence? expr) 
  (eq? expr #t))


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

  
;;;; Just a place holder at the moment
(define (beval-or expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (beval-not expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment  
(define (beval-imply expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment  
(define (beval-equiv expr env)
  (eq? expr #t))


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

;;;; (beval #t ()) ;Value: #t
;;;; (beval #f ()) ;Value: #f
;;;; (beval 'p '((p #t))) ;Value: #t  
;;;; (beval 'p '((a #t) (p #f))) ;Value: #f
;;;; (beval 'p '()) ;Value: error
;;;; (beval '(and #t p) '((p #t))) ;Value: #t  
;;;; (beval '(and p q r) '((p #t) (q #t) (r #t))) ;Value: #t  
;;;; (beval '(or p q r) '((p #t) (q #f) (r #f))) ;Value: #t  
;;;; (beval '(imply p q) '((p #t) (q #f))) ;Value: #f  
;;;; (beval '(imply p q) '((p #t) (q #t))) ;Value: #t  
;;;; (beval '(equiv (imply p q) (or (not p) q)) '((p #t) (q #f))) ;Value: #t 
;;;; (contains-only-true? '(#t #f #t #t #t))
  
  