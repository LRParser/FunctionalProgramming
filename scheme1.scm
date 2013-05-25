;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements an interpreter for the proposition calculus using scheme.
;;;;
;;;;    Name:       scheme1.scm
;;;;    Purpose:    Runs a test suite for beval.scm
;;;;
;;;;    Authors:    Jon Boone
;;;;		    Joshua Datko
;;;;		    Paul DeMicco
;;;;		    Joseph Heenan
;;;;
;;;;    Created:    05/21/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "beval.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Sample Inputs For Beval ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (run-test '(beval '(equiv (imply p q) (imply q p)) '((p #t) (q #f))) my-env)) ;Value: t

  
  