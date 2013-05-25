;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Implements an interpreter for the proposition calculus using scheme.
;;;;
;;;;    Name:       scheme2.scm
;;;;    Purpose:    Implements a tautology prover for the proposition calculus using scheme.
;;;;
;;;;    Authors:    Jon Boone
;;;;		    Joshua Datko
;;;;		    Paul DeMicco
;;;;		    Joseph Heenan
;;;;
;;;;    Created:    05/25/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; import the beval implementaion
(load "beval.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tautology prover function ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tautology-prover tautology)
  ;;; first, walk the tautology expr to find all the variables
  (let ((symbol-table (find-variables tautology)))
  ;;; then set up environments matching all possible combinations 
  ;;; of settings (#t/#f) for the variables 
  ;;; then call (beval tautology ...) with each of the environments
  ;;; collect the resutls and determine if they are all #t
    symbol-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; flatten an expr to aid in finding variables   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flatten expr)
  (let ((flat-expr '()))
    (if (null? expr)
        flat-expr
        (let ((first-element (car expr))
              (rest (cdr expr)))
          (if (list? first-element)
              (append (append (flatten first-element) flat-expr)
                      (flatten rest) flat-expr)
              (cons first-element (flatten rest)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; non-reserved function          ;;;;
;;;; finds non-reserved symbols in  ;;;;
;;;; boolean exprs                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (non-reserved symbol)
  (cond
   ((eq? symbol 'and) #f)
   ((eq? symbol 'or) #f)
   ((eq? symbol 'not) #f)
   ((eq? symbol 'imply) #f)
   ((eq? symbol 'equiv) #f)
   (else symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; not-false            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (not-false? x)
  (not (eq? x #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; uniq - eliminates redundant symbols in a list ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (uniq list)
  (if (null? list)
      '()
      (let ((seen-symbols (uniq (cdr list)))
            (first-symbol (car list)))
        (if (not (memq first-symbol seen-symbols))
            (cons first-symbol seen-symbols)
            seen-symbols))))
        
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Find-varaibles function   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-variables expr)
  (let* ((flat-expr (flatten expr))
         (symbol-list (filter not-false? (map non-reserved flat-expr))))
    (uniq symbol-list)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   Sample Inputs      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-test test-expression env)
  (pp test-expression)
  (write-string ";Value: ")
  (write-line (eval test-expression env)))

(let ((my-env (the-environment)))
  (run-test '(tautology-prover '(or p (not p))) my-env))  ;Value: #t  

  