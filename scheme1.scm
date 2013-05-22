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

;;;; Just a place holder at the moment
(define (conjuct? expr) 
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (disjuct? expr) 
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (negation? expr) 
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (implication? expr) 
  (eq? expr #t))

;;;; Just a place holder at the moment
(define (equivalence? expr) 
  (eq? expr #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Evaluation Functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Just a place holder at the moment
(define ( lookup expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment
(define ( beval-and expr env)
  (eq? expr #t))
  
;;;; Just a place holder at the moment
(define ( beval-or expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment
(define ( beval-not expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment  
(define ( beval-imply expr env)
  (eq? expr #t))

;;;; Just a place holder at the moment  
(define ( beval-equiv expr env)
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
;;;; (beval 'p '((p #t))) ;Value: #t  
;;;; (beval '(and #t p) '((p #t))) ;Value: #t  
;;;; (beval '(and p q r) '((p #t) (q #t) (r #t))) ;Value: #t  
;;;; (beval '(or p q r) '((p #t) (q #f) (r #f))) ;Value: #t  
;;;; (beval '(imply p q) '((p #t) (q #f))) ;Value: #f  
;;;; (beval '(imply p q) '((p #t) (q #t))) ;Value: #t  
;;;; (beval '(equiv (imply p q) (or (not p) q)) '((p #t) (q #f))) ;Value: #t 
;;;; (contains-only-true? '(#t #f #t #t #t))
  
  