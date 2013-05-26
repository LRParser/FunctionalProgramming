;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements test cases for the tautology prover 
;;;;
;;;;    Name:       scheme2.scm
;;;;    Purpose:    Implements test cases for the tautology prover
;;;;
;;;;    Authors:    Jon Boone
;;;;		    Joshua Datko
;;;;		    Paul DeMicco
;;;;		    Joseph Heenan
;;;;
;;;;    Created:    05/25/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load the tautology prover
(load "tautprover.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Sample Inputs      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-test test-expression env)
  (pp test-expression)
  (write-string ";Value: ")
  (write-line (eval test-expression env)))

(let ((my-env (the-environment)))

  ; Samples from Professor Johnson - https://www.cs.drexel.edu/~jjohnson/2012-13/spring/cs550/assignments/assign5.html
  (run-test '(tautology-prover '(or p (not p))) my-env) ;Value: #t
  (run-test '(tautology-prover '(equiv (or p q) (or q p))) my-env) ;Value: #t
  (run-test '(tautology-prover '(equiv (or P Q) (or P (and (not P) Q)))) my-env) ;Value: #t
  (run-test '(tautology-prover '(and a b)) my-env) ;Value: #f

  ; Identity Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(and p #t) p)) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(or p #f) p)) my-env);Value: #t

  ; Domination Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or p #t) #t)) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(and p #f) #f)) my-env);Value: #t

  ; Idempotent Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or p p) p)) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(and p p) p)) my-env);Value: #t

  ; Double Negation Law - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(not(not p))p)) my-env);Value: #t

  ; Commutative Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or p q)(or q p))) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(and p q)(and q p))) my-env);Value: #t

  ; Associative Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or(or p q) r) (or(or q r) p))) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(and(and p q) r) (and(and q r) p))) my-env);Value: #t

  ; Distributive Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or(and q r) p)(and(or p q)(or p r)))) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(and(or q r) p)(or(and p q)(and p r)))) my-env);Value: #t
 
  ; De Morgan's Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(not(and p q))(or(not p)(not q)))) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(not(or p q))(and(not p)(not q)))) my-env);Value: #t
 
  ; Absorption Laws - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(and(or p q)p) p)) my-env);Value: #t
  (run-test '(tautology-prover '(equiv(or(and p q)p) p)) my-env);Value: #t

  ; Implication Law - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv (imply p q) (or (not p) q))) my-env);Value: #t

  ; Contrapositive Law - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(imply p q)(imply(not q)(not p)))) my-env);Value: #t

  ; Tautology - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(or(not p) p) #t)) my-env);Value: #t

  ; Contradiction - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(and(not p) p) #f)) my-env);Value: #t

  ; Equivalence - http://people.math.gatech.edu/~ecroot/2406_2012/basic_logic.pdf
  (run-test '(tautology-prover '(equiv(and(imply p q)(imply q p))(equiv p q))) my-env);Value: #t
)

